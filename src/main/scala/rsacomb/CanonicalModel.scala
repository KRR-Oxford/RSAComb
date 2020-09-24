package rsacomb

import org.semanticweb.owlapi.model.{
  OWLSubObjectPropertyOfAxiom,
  OWLSubClassOfAxiom,
  OWLObjectPropertyExpression
}
import tech.oxfordsemantic.jrdfox.logic.{Rule, Term}

object ProgramGenerator {

  def apply(
      term: Term,
      unsafe: List[OWLObjectPropertyExpression] = List()
  ): RDFoxAxiomConverter =
    new ProgramGenerator(term, unsafe)

  def generateRoleRules(
      roles: List[OWLObjectPropertyExpression]
  ): List[Rule] = {
    // TODO: Generate additional rules for each role
    val additional = List()
    additional
  }
}

class ProgramGenerator(
    term: Term,
    unsafe: List[OWLObjectPropertyExpression]
) extends RDFoxAxiomConverter(term, SkolemStrategy.None, unsafe)
    with RSAAxiom {

  override def visit(axiom: OWLSubClassOfAxiom): List[Rule] = {
    if (axiom.isT5) {
      // TODO: get role in T5 axiom
      // Assuming one role here
      val role = axiom.objectPropertyExpressionsInSignature(0)
      if (unsafe.contains(role)) {
        val visitor =
          new RDFoxAxiomConverter(term, SkolemStrategy.Standard("TODO"), unsafe)
        axiom.accept(visitor)
      } else {
        // TODO; Handle forks
      }
    } else {
      super.visit(axiom)
    }
    List()
  }

  override def visit(axiom: OWLSubObjectPropertyOfAxiom): List[Rule] = {
    // TODO: Generate additional rules for role inclusion
    val additional = List()
    super.visit(axiom) ++ additional
  }

}
