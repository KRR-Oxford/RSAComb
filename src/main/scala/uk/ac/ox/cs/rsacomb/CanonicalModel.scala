package uk.ac.ox.cs.rsacomb

import org.semanticweb.owlapi.model.{OWLObjectInverseOf, OWLObjectProperty}
import org.semanticweb.owlapi.model.{
  OWLClass,
  OWLLogicalAxiom,
  // OWLObjectProperty,
  OWLSubObjectPropertyOfAxiom,
  OWLObjectPropertyExpression,
  OWLObjectSomeValuesFrom,
  OWLSubClassOfAxiom
}

import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Rule,
  BodyFormula,
  TupleTableAtom,
  Negation
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  Term,
  Variable,
  // Resource,
  IRI
}

import implicits.JavaCollections._

import uk.ac.ox.cs.rsacomb.converter._
import uk.ac.ox.cs.rsacomb.suffix._
import uk.ac.ox.cs.rsacomb.util.RSA

/** Canonical model generator
  *
  * Converts the input axioms in a given ontology into logic rules that
  * can then be passed to RDFox to compute the actual canonical model
  * (via materialization).
  *
  * @param ontology the RSA ontology the canonical model is targeting.
  */
class CanonicalModel(val ontology: RSAOntology) {

  /** Simplify conversion between OWLAPI and RDFox concepts */
  import implicits.RDFox._

  /** Extends capabilities of
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]]
    */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._

  /** Introduce additional rules for each role.
    *
    * Some relations between roles and their inverse or their "suffixed"
    * versions need to be explicitly stated in terms of logic rules.
    */
  val rolesAdditionalRules: List[Rule] = {
    ontology.roles
      .collect { case prop: OWLObjectProperty => prop }
      .flatMap((pred) => {
        val iri = pred.getIRI.getIRIString
        val (varX, varY) = (Variable.create("X"), Variable.create("Y"))
        for (
          (hSuffix, bSuffix) <- Seq(
            (Empty, Forward),
            (Empty, Backward),
            (Inverse, Forward + Inverse),
            (Inverse, Backward + Inverse),
            (Backward + Inverse, Forward),
            (Forward + Inverse, Backward),
            (Backward, Forward + Inverse),
            (Forward, Backward + Inverse)
          )
        )
          yield Rule.create(
            TupleTableAtom.rdf(varX, iri :: hSuffix, varY),
            TupleTableAtom.rdf(varX, iri :: bSuffix, varY)
          )
      })
  }

  /** Top axiomatization
    *
    * Corresponding to the following rules:
    *
    * ```
    *   [?a, rdf:type, owl:Thing] :- [?a, rdf:type, ?b] .
    *   [?a, rdf:type, owl:Thing], [?b, rdf:type, owl:Thing] :- [?a, ?r, ?b], FILTER(?r != rdf:type).
    * ```
    *
    * @note this is a naïve implementation of top axiomatization and
    * might change in the future. The ideal solution would be for RDFox
    * to take care of this, but at the time of writing this is not
    * compatible with the way we are using the tool.
    */
  private val topAxioms: List[Rule] = {
    val varA = Variable.create("A")
    val varR = Variable.create("R")
    val varB = Variable.create("B")
    List(
      Rule.create(
        RSA.Thing(varA),
        TupleTableAtom.rdf(varA, IRI.RDF_TYPE, varB)
      ),
      Rule.create(
        List(RSA.Thing(varA), RSA.Thing(varB)),
        List(
          TupleTableAtom.rdf(varA, varR, varB),
          FilterAtom.create(FunctionCall.notEqual(varR, IRI.RDF_TYPE))
        )
      )
    )
  }

  /** Equality axiomatization
    *
    * Introduce reflexivity, simmetry and transitivity rules for a naïve
    * equality axiomatization.
    *
    * @note that we are using a custom `congruent` predicate to indicate
    * equality. This is to avoid interfering with the standard
    * `owl:sameAs`.
    *
    * @note RDFox is able to handle equality in a "smart" way, but this
    * behaviour is incompatible with other needed features like
    * negation-as-failure and aggregates.
    *
    * @todo to complete the equality axiomatization we need to introduce
    * substitution rules to explicate a complete "equality" semantics.
    */
  private val equalityAxioms: List[Rule] = {
    val varX = Variable.create("X")
    val varY = Variable.create("Y")
    val varZ = Variable.create("Z")
    List(
      // Reflexivity
      Rule.create(RSA.Congruent(varX, varX), RSA.Thing(varX)),
      // Simmetry
      Rule.create(RSA.Congruent(varY, varX), RSA.Congruent(varX, varY)),
      // Transitivity
      Rule.create(
        RSA.Congruent(varX, varZ),
        RSA.Congruent(varX, varY),
        RSA.Congruent(varY, varZ)
      )
    )
  }

  val (facts, rules): (List[TupleTableAtom], List[Rule]) = {
    // Compute rules from ontology axioms
    val (facts, rules) = {
      val term = RSAOntology.genFreshVariable()
      val unsafe = ontology.unsafeRoles
      ontology.axioms
        .map(CanonicalModelConverter.convert(_, term, unsafe, NoSkolem, Empty))
        .unzip
    }
    (
      facts.flatten,
      rolesAdditionalRules ::: topAxioms ::: equalityAxioms ::: rules.flatten
    )
  }

  object CanonicalModelConverter extends RDFoxConverter {

    private def rules1(
        axiom: OWLSubClassOfAxiom
    ): Result = {
      val unfold = ontology.unfold(axiom).toList
      // Fresh Variables
      val v0 = RSA("v0_" ++ axiom.hashed)
      val varX = Variable.create("X")
      implicit val unfoldTerm = RSA(unfold.hashCode.toString)
      // TODO: use axiom.toTriple instead
      val atomA: TupleTableAtom = {
        val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
        TupleTableAtom.rdf(varX, IRI.RDF_TYPE, cls)
      }
      val roleRf: TupleTableAtom = {
        val prop =
          axiom.getSuperClass.asInstanceOf[OWLObjectSomeValuesFrom].getProperty
        super.convert(prop, varX, v0, Forward)
      }
      val atomB: TupleTableAtom = {
        val cls = axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getFiller
          .asInstanceOf[OWLClass]
          .getIRI
        TupleTableAtom.rdf(v0, IRI.RDF_TYPE, cls)
      }
      // TODO: To be consistent with the specifics of the visitor we are
      // returning facts as `Rule`s with true body. While this is correct
      // there is an easier way to import facts into RDFox. Are we able to
      // do that?
      val facts = unfold map RSA.In
      val rules = List(
        Rule.create(roleRf, atomA, RSA.NotIn(varX)),
        Rule.create(atomB, atomA, RSA.NotIn(varX))
      )
      (facts, rules)
    }

    private def rules2(axiom: OWLSubClassOfAxiom): List[Rule] = {
      val roleR =
        axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getProperty
      if (ontology.confl(roleR) contains roleR) {
        // Fresh Variables
        val v0 = RSA("v0_" ++ axiom.hashed)
        val v1 = RSA("v1_" ++ axiom.hashed)
        val v2 = RSA("v2_" ++ axiom.hashed)
        // Predicates
        def atomA(t: Term): TupleTableAtom = {
          val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
          TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
        }
        def roleRf(t1: Term, t2: Term): TupleTableAtom =
          super.convert(roleR, t1, t2, Forward)
        def atomB(t: Term): TupleTableAtom = {
          val cls = axiom.getSuperClass
            .asInstanceOf[OWLObjectSomeValuesFrom]
            .getFiller
            .asInstanceOf[OWLClass]
            .getIRI
          TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
        }
        //Rules
        List(
          Rule.create(roleRf(v0, v1), atomA(v0)),
          Rule.create(atomB(v1), atomA(v0)),
          Rule.create(roleRf(v1, v2), atomA(v1)),
          Rule.create(atomB(v2), atomA(v1))
        )
      } else {
        List()
      }
    }

    private def rules3(axiom: OWLSubClassOfAxiom): List[Rule] = {
      val cycle = ontology.cycle(axiom).toList
      val roleR =
        axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getProperty
      // Fresh Variables
      val v1 = RSA("v1_" ++ axiom.hashed)
      // Predicates
      def atomA(t: Term): TupleTableAtom = {
        val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
        TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
      }
      def roleRf(t: Term): TupleTableAtom =
        super.convert(roleR, t, v1, Forward)
      val atomB: TupleTableAtom = {
        val cls = axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getFiller
          .asInstanceOf[OWLClass]
          .getIRI
        TupleTableAtom.rdf(v1, IRI.RDF_TYPE, cls)
      }
      cycle.flatMap { x =>
        List(
          Rule.create(roleRf(x), atomA(x)),
          Rule.create(atomB, atomA(x))
        )
      }
    }

    override def convert(
        axiom: OWLLogicalAxiom,
        term: Term,
        unsafe: List[OWLObjectPropertyExpression],
        skolem: SkolemStrategy,
        suffix: RSASuffix
    ): Result =
      axiom match {

        case a: OWLSubClassOfAxiom if a.isT5 => {
          val role = axiom.objectPropertyExpressionsInSignature(0)
          if (unsafe contains role)
            super.convert(a, term, unsafe, new Standard(a), Forward)
          else {
            val (f1, r1) = rules1(a)
            (f1, r1 ::: rules2(a) ::: rules3(a))
          }
        }

        case a: OWLSubObjectPropertyOfAxiom => {
          val (facts, rules) = List(Empty, Forward, Backward)
            .map(super.convert(a, term, unsafe, NoSkolem, _))
            .unzip
          (facts.flatten, rules.flatten)
        }

        case a => super.convert(a, term, unsafe, skolem, suffix)

      }
  }

}
