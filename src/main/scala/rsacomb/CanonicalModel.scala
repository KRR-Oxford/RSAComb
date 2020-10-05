package rsacomb

import org.semanticweb.owlapi.model.{
  OWLOntology,
  OWLClass,
  OWLSubObjectPropertyOfAxiom,
  OWLSubClassOfAxiom,
  OWLObjectProperty,
  OWLObjectPropertyExpression,
  OWLObjectSomeValuesFrom
}
import tech.oxfordsemantic.jrdfox.logic.{
  IRI,
  BodyFormula,
  Atom,
  Rule,
  Term,
  Variable
}
import scala.collection.JavaConverters._

object ProgramGenerator {

  def apply(
      ontology: OWLOntology,
      term: Term,
      unsafe: List[OWLObjectPropertyExpression] = List()
  ): RDFoxAxiomConverter =
    new ProgramGenerator(ontology, term, unsafe)

  def generateRoleRules(
      roles: List[OWLObjectPropertyExpression]
  ): List[Rule] = {
    def additional(pred: String): Seq[Rule] = {
      val varX = Variable.create("X")
      val varY = Variable.create("Y")
      Seq(
        Rule.create(
          Atom.rdf(varX, IRI.create(pred), varY),
          Atom.rdf(varX, IRI.create(pred ++ RSASuffix.Forward.getSuffix), varY)
        ),
        Rule.create(
          Atom.rdf(varX, IRI.create(pred), varY),
          Atom.rdf(varX, IRI.create(pred ++ RSASuffix.Backward.getSuffix), varY)
        ),
        Rule.create(
          Atom.rdf(
            varY,
            IRI.create(pred ++ RSASuffix.Backward.getSuffix ++ "_inv"),
            varX
          ),
          Atom.rdf(varX, IRI.create(pred ++ RSASuffix.Forward.getSuffix), varY)
        ),
        Rule.create(
          Atom.rdf(
            varY,
            IRI.create(pred ++ RSASuffix.Forward.getSuffix ++ "_inv"),
            varX
          ),
          Atom.rdf(varX, IRI.create(pred ++ RSASuffix.Backward.getSuffix), varY)
        )
      )
    }
    roles
      .filter(_.isInstanceOf[OWLObjectProperty]) // Can we avoid this?
      .map(_.asInstanceOf[OWLObjectProperty].getIRI.getIRIString)
      .flatMap(additional)
  }
}

class ProgramGenerator(
    ontology: OWLOntology,
    term: Term,
    unsafe: List[OWLObjectPropertyExpression]
) extends RDFoxAxiomConverter(term, unsafe, SkolemStrategy.None, RSASuffix.None)
    with RSAOntology
    with RSAAxiom {

  import RDFoxUtil._

  def rules1(axiom: OWLSubClassOfAxiom): (List[Rule], List[Atom]) = {
    val unfold = ontology.cycle(axiom).toList
    // Fresh Variables
    val v0 = IRI.create("v0_" ++ axiom.hashCode.toString)
    val varX = Variable.create("x")
    // Predicates
    val atomA: Atom = {
      val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
      Atom.rdf(varX, IRI.RDF_TYPE, cls)
    }
    def notIn(t: Term): Atom = {
      Atom.rdf(
        t,
        RSA.internal("notIn"),
        RSA.internal(unfold.hashCode.toString)
      )
    }
    val roleRf: Atom = {
      val visitor =
        new RDFoxPropertyExprConverter(varX, v0, RSASuffix.Forward)
      axiom.getSuperClass
        .asInstanceOf[OWLObjectSomeValuesFrom]
        .getProperty
        .accept(visitor)
        .head
    }
    val atomB: Atom = {
      val cls = axiom.getSuperClass
        .asInstanceOf[OWLObjectSomeValuesFrom]
        .getFiller
        .asInstanceOf[OWLClass]
        .getIRI
      Atom.rdf(v0, IRI.RDF_TYPE, cls)
    }
    (
      List(
        Rule.create(roleRf, atomA, notIn(varX)),
        Rule.create(atomB, atomA, notIn(varX))
      ),
      unfold map notIn
    )
  }

  def rules2(axiom: OWLSubClassOfAxiom): List[Rule] = {
    val roleR =
      axiom.getSuperClass
        .asInstanceOf[OWLObjectSomeValuesFrom]
        .getProperty
    if (ontology.confl(roleR) contains roleR) {
      // Fresh Variables
      val v0 = IRI.create("v0_" ++ axiom.hashCode.toString)
      val v1 = IRI.create("v1_" ++ axiom.hashCode.toString)
      val v2 = IRI.create("v2_" ++ axiom.hashCode.toString)
      // Predicates
      def atomA(t: Term): Atom = {
        val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
        Atom.rdf(t, IRI.RDF_TYPE, cls)
      }
      def roleRf(t1: Term, t2: Term): Atom = {
        val visitor = new RDFoxPropertyExprConverter(t1, t2, RSASuffix.Forward)
        roleR.accept(visitor).head
      }
      def atomB(t: Term): Atom = {
        val cls = axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getFiller
          .asInstanceOf[OWLClass]
          .getIRI
        Atom.rdf(t, IRI.RDF_TYPE, cls)
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

  def rules3(axiom: OWLSubClassOfAxiom): List[Rule] = {
    val cycle = ontology.cycle(axiom).toList
    val roleR =
      axiom.getSuperClass
        .asInstanceOf[OWLObjectSomeValuesFrom]
        .getProperty
    // Fresh Variables
    val v1 = IRI.create("v1_" ++ axiom.hashCode.toString)
    // Predicates
    def atomA(t: Term): Atom = {
      val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
      Atom.rdf(t, IRI.RDF_TYPE, cls)
    }
    def roleRf(t: Term): Atom = {
      val visitor =
        new RDFoxPropertyExprConverter(t, v1, RSASuffix.Forward)
      roleR.accept(visitor).head
    }
    val atomB: Atom = {
      val cls = axiom.getSuperClass
        .asInstanceOf[OWLObjectSomeValuesFrom]
        .getFiller
        .asInstanceOf[OWLClass]
        .getIRI
      Atom.rdf(v1, IRI.RDF_TYPE, cls)
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
      if (unsafe.contains(role)) {
        val visitor =
          new RDFoxAxiomConverter(
            term,
            unsafe,
            SkolemStrategy.Standard(axiom.toString),
            RSASuffix.Forward
          )
        axiom.accept(visitor)
      } else {
        val (r1, f1) = rules1(axiom)
        val r2 = rules2(axiom)
        val r3 = rules3(axiom)
        r1 ++ r2 ++ r3
      }
    } else {
      // Fallback to standard OWL to LP translation
      super.visit(axiom)
    }
  }

  override def visit(axiom: OWLSubObjectPropertyOfAxiom): List[Rule] = {
    // TODO: Generate additional rules for role inclusion
    val additional = List()
    super.visit(axiom) ++ additional
  }

}
