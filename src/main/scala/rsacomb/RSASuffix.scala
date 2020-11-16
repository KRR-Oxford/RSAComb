package rsacomb.suffix

import org.semanticweb.owlapi.model.{
  OWLPropertyExpression,
  OWLObjectInverseOf,
  OWLObjectProperty
}

class RSASuffix(val suffix: String => String) {

  def +(other: RSASuffix): RSASuffix =
    new RSASuffix((s: String) => other suffix (this suffix s))

  def ::(str: String): String = this suffix str

  def ::(expr: OWLPropertyExpression): String =
    expr match {
      case e: OWLObjectProperty  => e.getIRI.getIRIString :: this
      case e: OWLObjectInverseOf => e.getInverse :: this
    }

}

case object Empty extends RSASuffix((x) => x)
case object Forward extends RSASuffix((s) => s"${s}_f")
case object Backward extends RSASuffix((s) => s"${s}_b")
case object Inverse extends RSASuffix((s) => s"${s}_inv")
case class Nth(n: Int) extends RSASuffix((s) => s"${s}_$n")
