package rsacomb

import org.semanticweb.owlapi.model.{
  OWLSubObjectPropertyOfAxiom,
  OWLSubClassOfAxiom,
  OWLObjectProperty,
  OWLObjectPropertyExpression
}
import tech.oxfordsemantic.jrdfox.logic.{IRI, Atom, Rule, Term, Variable}

object ProgramGenerator {

  def apply(
      term: Term,
      unsafe: List[OWLObjectPropertyExpression] = List()
  ): RDFoxAxiomConverter =
    new ProgramGenerator(term, unsafe)

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
    term: Term,
    unsafe: List[OWLObjectPropertyExpression]
) extends RDFoxAxiomConverter(term, unsafe, SkolemStrategy.None, RSASuffix.None)
    with RSAAxiom {

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
        List()
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
