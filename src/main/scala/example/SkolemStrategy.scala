package rsacomb

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
  case class Standard(func : String) extends SkolemStrategy
  object Standard {
    def apply(axiom : String) = new Standard(genFunctionString(axiom))
    def genFunctionString(str : String) = "f_" ++ str.hashCode.toString
  }

  /* Functional skolemization
   *
   * From 
   *    A ⊑ ∃R.B
   * to
   *    A(y) -> R(x,c), B(c)
   * for c, fresh constant associated with the input axiom
   */
  case class Constant(const : String) extends SkolemStrategy
  object Constant {
    def apply(axiom : String) = new Constant(genConstantString(axiom))
    def genConstantString(str : String) = "internal:c_" ++ str.hashCode.toString
  }
}

