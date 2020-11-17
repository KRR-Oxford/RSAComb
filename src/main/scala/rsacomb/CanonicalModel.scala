package rsacomb

import org.semanticweb.owlapi.model.{OWLObjectInverseOf, OWLObjectProperty}
import org.semanticweb.owlapi.model.{
  OWLClass,
  // OWLObjectProperty,
  OWLSubObjectPropertyOfAxiom,
  // OWLObjectPropertyExpression,
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

import suffix.{Empty, Forward, Backward, Inverse}

class CanonicalModel(val ontology: RSAOntology) extends RSAAxiom {

  // Makes working with IRIs less painful
  import RDFoxUtil._

  val named: List[Rule] =
    ontology.individuals.map(a =>
      Rule.create(TupleTableAtom.rdf(a, IRI.RDF_TYPE, RSA.Named))
    )

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
        TupleTableAtom.rdf(varX, IRI.RDF_TYPE, IRI.THING),
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
        List(
          TupleTableAtom.rdf(varX, IRI.RDF_TYPE, IRI.THING),
          TupleTableAtom.rdf(varY, IRI.RDF_TYPE, IRI.THING)
        ),
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
      Rule.create(
        TupleTableAtom.rdf(varX, RSA.EquivTo, varX),
        TupleTableAtom.rdf(varX, IRI.RDF_TYPE, IRI.THING)
      ),
      Rule.create(
        TupleTableAtom.rdf(varY, RSA.EquivTo, varX),
        TupleTableAtom.rdf(varX, RSA.EquivTo, varY)
      ),
      Rule.create(
        TupleTableAtom.rdf(varX, RSA.EquivTo, varZ),
        TupleTableAtom.rdf(varX, RSA.EquivTo, varY),
        TupleTableAtom.rdf(varY, RSA.EquivTo, varZ)
      )
    )
  }

  val rules: List[Rule] = {
    // Compute rules from ontology axioms
    val rules = ontology.axioms.flatMap(_.accept(RuleGenerator))
    // Return full set of rules
    rules ::: rolesAdditionalRules ::: topAxioms ::: equalityAxioms ::: named
  }

  object RuleGenerator
      extends RDFoxAxiomConverter(
        Variable.create("X"),
        ontology.unsafeRoles,
        SkolemStrategy.None,
        Empty
      ) {

    private def rules1(axiom: OWLSubClassOfAxiom): List[Rule] = {
      val unfold = ontology.unfold(axiom).toList
      // Fresh Variables
      val v0 = RSA.rsa("v0_" ++ RSA.hashed(axiom))
      val varX = Variable.create("X")
      // Predicates
      val atomA: TupleTableAtom = {
        val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
        TupleTableAtom.rdf(varX, IRI.RDF_TYPE, cls)
      }
      def predIN(t: Term): TupleTableAtom = {
        TupleTableAtom.rdf(
          t,
          RSA.rsa("IN"),
          RSA.rsa(unfold.hashCode.toString)
        )
      }
      def notIn(t: Term): Negation = Negation.create(predIN(t))
      val roleRf: TupleTableAtom = {
        val visitor =
          new RDFoxPropertyExprConverter(varX, v0, Forward)
        axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getProperty
          .accept(visitor)
          .head
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
      val facts = unfold.map(x => Rule.create(predIN(x)))
      val rules = List(
        Rule.create(roleRf, atomA, notIn(varX)),
        Rule.create(atomB, atomA, notIn(varX))
      )
      facts ++ rules
    }

    private def rules2(axiom: OWLSubClassOfAxiom): List[Rule] = {
      val roleR =
        axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getProperty
      if (ontology.confl(roleR) contains roleR) {
        // Fresh Variables
        val v0 = RSA.rsa("v0_" ++ RSA.hashed(axiom))
        val v1 = RSA.rsa("v1_" ++ RSA.hashed(axiom))
        val v2 = RSA.rsa("v2_" ++ RSA.hashed(axiom))
        // Predicates
        def atomA(t: Term): TupleTableAtom = {
          val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
          TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
        }
        def roleRf(t1: Term, t2: Term): TupleTableAtom = {
          val visitor =
            new RDFoxPropertyExprConverter(t1, t2, Forward)
          roleR.accept(visitor).head
        }
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
      val v1 = RSA.rsa("v1_" ++ RSA.hashed(axiom))
      // Predicates
      def atomA(t: Term): TupleTableAtom = {
        val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
        TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
      }
      def roleRf(t: Term): TupleTableAtom = {
        val visitor =
          new RDFoxPropertyExprConverter(t, v1, Forward)
        roleR.accept(visitor).head
      }
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

    override def visit(axiom: OWLSubClassOfAxiom): List[Rule] = {
      if (axiom.isT5) {
        // TODO: get role in T5 axiom
        // Assuming one role here
        val role = axiom.objectPropertyExpressionsInSignature(0)
        if (ontology.unsafeRoles contains role) {
          val visitor =
            new RDFoxAxiomConverter(
              Variable.create("X"),
              ontology.unsafeRoles,
              SkolemStrategy.Standard(axiom.toString),
              Forward
            )
          axiom.accept(visitor)
        } else {
          rules1(axiom) ::: rules2(axiom) ::: rules3(axiom)
        }
      } else {
        // Fallback to standard OWL to LP translation
        super.visit(axiom)
      }
    }

    override def visit(axiom: OWLSubObjectPropertyOfAxiom): List[Rule] = {
      val varX = Variable.create("X")
      val visitorF = new RDFoxAxiomConverter(
        varX,
        ontology.unsafeRoles,
        SkolemStrategy.None,
        Forward
      )
      val visitorB = new RDFoxAxiomConverter(
        varX,
        ontology.unsafeRoles,
        SkolemStrategy.None,
        Backward
      )
      axiom.accept(visitorB) ::: axiom.accept(visitorF)
    }

  }

}
