package rsacomb.suffix

import org.semanticweb.owlapi.model.{
  OWLPropertyExpression,
  OWLObjectInverseOf,
  OWLObjectProperty
}

import tech.oxfordsemantic.jrdfox.logic.expression.{IRI}
import tech.oxfordsemantic.jrdfox.logic.datalog.{TupleTableAtom, TupleTableName}

object RSASuffix {

  def apply(suffix: String => String): RSASuffix = new RSASuffix(suffix)

}

class RSASuffix(val suffix: String => String) {

  def +(that: RSASuffix): RSASuffix =
    new RSASuffix(this.suffix andThen that.suffix)

  def ::(str: String): String = this suffix str

}

case object Empty extends RSASuffix(identity)
case object Forward extends RSASuffix((s) => s"${s}_f")
case object Backward extends RSASuffix((s) => s"${s}_b")
case object Inverse extends RSASuffix((s) => s"${s}_inv")
case class Nth(n: Int) extends RSASuffix((s) => s"${s}_$n")
