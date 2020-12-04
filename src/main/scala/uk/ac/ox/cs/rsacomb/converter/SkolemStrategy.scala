package uk.ac.ox.cs.rsacomb.converter

import org.semanticweb.owlapi.model.OWLAxiom
import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.expression.{Literal, IRI}

sealed trait SkolemStrategy

/** No skolemization.
  *
  * @example role `R` from
  * {{{
  *   ∃R.A ⊑ B
  * }}}
  * to
  * {{{
  *   R(x,y), B(y) -> B(x)
  * }}}
  */
case object NoSkolem extends SkolemStrategy

/** Functional skolemization
  *
  * The factory object should be used to create new instances of the
  * class.
  *
  * @example role `R` from
  * {{{
  *   A ⊑ ∃R.B
  * }}}
  * to
  * {{{
  *   A(x) -> R(x,f(x)), B(f(x))
  * }}}
  * for `f`, fresh function uniquely associated with the input axiom.
  *
  * RDFox does not support function symbols. We can still implement
  * function symbols combining the `BIND` operator with the `SKOLEM`
  * operator as such:
  * {{{
  *   A(x), BIND(y, SKOLEM("f", x)) -> R(x,y), B(y)
  * }}}
  * The first argument of a `SKOLEM` call '''must''' be a literal string
  * (ideally identifing the simulated function name).
  *
  * @note this requirement is not enforced by RDFox, that will fail
  * silently if a string argument is omitted.
  */
class Standard(var axiom: OWLAxiom)(implicit toString: (OWLAxiom) => String)
    extends SkolemStrategy {
  def dup(a: OWLAxiom) = new Standard(a)(toString)
  lazy val literal =
    Literal.create(s"f_${toString(axiom)}", Datatype.XSD_STRING)
}

/** Constant skolemization
  *
  * The factory object should be used to create new instances of the
  * class.
  *
  * @example role `R` from
  * {{{
  *   A ⊑ ∃R.B
  * }}}
  * to
  * {{{
  *   A(y) -> R(x,c), B(c)
  * }}}
  * for `c`, fresh constant '''uniquely''' associated with the input
  * axiom
  */
class Constant(var axiom: OWLAxiom)(implicit toString: (OWLAxiom) => String)
    extends SkolemStrategy {
  def dup(a: OWLAxiom) = new Constant(a)(toString)
  lazy val iri = IRI.create(s"c_${toString(axiom)}")
}
