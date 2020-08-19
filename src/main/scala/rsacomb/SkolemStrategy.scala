package rsacomb

import tech.oxfordsemantic.jrdfox.logic.IRI

sealed trait SkolemStrategy

object SkolemStrategy {
  // TODO: might want to use something else other than `hashCode` as a
  // function to generate a fresh function/constant

  /* No skolemization at all.
   *
   * From
   *    ∃R.A ⊑ B
   * to
   *    R(x,y), B(y) -> B(x)
   */
  case object None extends SkolemStrategy

  /* Functional skolemization
   *
   * From
   *    A ⊑ ∃R.B
   * to
   *    A(y) -> R(x,f(x)), B(f(x))
   * for f, fresh function associated with the input axiom
   */
  case class Standard(func: IRI) extends SkolemStrategy
  object Standard {
    def apply(axiom: String) =
      new Standard(IRI.create(genFunctionString(axiom)))
    def genFunctionString(str: String) = "f_" ++ str.hashCode.toString
  }

  /* Constant skolemization
   *
   * From
   *    A ⊑ ∃R.B
   * to
   *    A(y) -> R(x,c), B(c)
   * for c, fresh constant associated with the input axiom
   */
  case class Constant(const: IRI) extends SkolemStrategy
  object Constant {
    def apply(axiom: String) =
      new Constant(IRI.create(genConstantString(axiom)))
    def genConstantString(str: String) = "c_" ++ str.hashCode.toString
  }

  /* (RSA) Constant skolemization
   * This is a special skolemization option to introduce additional atoms for RSA
   * checking algorithm.
   *
   * From
   *    A ⊑ ∃R.B
   * to
   *    A(y) -> R(x,c), PE(x,c), B(c)
   * for c, fresh constant associated with the input axiom and PE an internal predicate.
   */
  case class ConstantRSA(const: IRI) extends SkolemStrategy
  object ConstantRSA {
    def apply(axiom: String) =
      new ConstantRSA(IRI.create(genConstantString(axiom)))
    def genConstantString(str: String) = "c_" ++ str.hashCode.toString
  }
}
