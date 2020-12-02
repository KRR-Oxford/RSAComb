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

import uk.ac.ox.cs.rsacomb.converter.{
  SkolemStrategy,
  RDFoxConverter
  // RDFoxAxiomConverter,
  // RDFoxPropertyExprConverter
}
import uk.ac.ox.cs.rsacomb.suffix._
import uk.ac.ox.cs.rsacomb.util.RSA

class CanonicalModel(val ontology: RSAOntology) {

  import implicits.RDFox._
  import implicits.JavaCollections._
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._

  val rolesAdditionalRules: List[Rule] = {
    // Given a role (predicate) compute additional logic rules
    def additional(pred: String): Seq[Rule] = {
      val varX = Variable.create("X")
      val varY = Variable.create("Y")
      for (
        (hSuffix, bSuffix) <- List(
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
          TupleTableAtom.rdf(varX, pred :: hSuffix, varY),
          TupleTableAtom.rdf(varX, pred :: bSuffix, varY)
        )
    }
    // Compute additional rules per role
    ontology.roles
      .collect { case prop: OWLObjectProperty => prop }
      .map(_.getIRI.getIRIString)
      .flatMap(additional)
  }

  private lazy val topAxioms: List[Rule] = {
    val varX = Variable.create("X")
    val varY = Variable.create("Y")
    val concepts = ontology.concepts.map(c => {
      Rule.create(
        RSA.Thing(varX),
        TupleTableAtom.rdf(varX, IRI.RDF_TYPE, c.getIRI)
      )
    })
    val roles = ontology.roles.map(r => {
      val name = r match {
        case x: OWLObjectProperty => x.getIRI.getIRIString
        case x: OWLObjectInverseOf =>
          x.getInverse.getNamedProperty.getIRI.getIRIString :: Inverse
      }
      Rule.create(
        List(RSA.Thing(varX), RSA.Thing(varY)),
        List(TupleTableAtom.rdf(varX, name, varY))
      )
    })
    concepts ::: roles
  }

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
      val skolem = SkolemStrategy.None
      val suffix = Empty
      ontology.axioms
        .map(CanonicalModelConverter.convert(_, term, unsafe, skolem, suffix))
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
          if (unsafe contains role) {
            val skolem = SkolemStrategy.Standard(a.toString)
            super.convert(a, term, unsafe, skolem, Forward)
          } else {
            val (f1, r1) = rules1(a)
            (f1, r1 ::: rules2(a) ::: rules3(a))
          }
        }

        case a: OWLSubObjectPropertyOfAxiom => {
          val (factsF, rulesF) =
            super.convert(a, term, unsafe, SkolemStrategy.None, Forward)
          val (factsB, rulesB) =
            super.convert(a, term, unsafe, SkolemStrategy.None, Backward)
          (factsF ::: factsB, rulesF ::: rulesB)
        }

        case a => super.convert(a, term, unsafe, skolem, suffix)

      }
  }

}
