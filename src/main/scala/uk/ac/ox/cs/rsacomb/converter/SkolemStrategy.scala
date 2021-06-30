/*
 * Copyright 2020, 2021 KRR Oxford
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ox.cs.rsacomb.converter

import org.semanticweb.owlapi.model.OWLAxiom
import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.expression.{Literal, IRI}

sealed trait SkolemStrategy {
  def dup(a: OWLAxiom): SkolemStrategy
}

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
case object NoSkolem extends SkolemStrategy {
  def dup(a: OWLAxiom): SkolemStrategy = this
}

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
case class Standard(axiom: OWLAxiom)(implicit toString: (OWLAxiom) => String)
    extends SkolemStrategy {
  def dup(_axiom: OWLAxiom): Standard = copy(axiom = _axiom)(toString)
  lazy val name = s"f_${toString(axiom)}"
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
case class Constant(axiom: OWLAxiom)(implicit toString: (OWLAxiom) => String)
    extends SkolemStrategy {
  def dup(_axiom: OWLAxiom): Constant = copy(axiom = _axiom)(toString)
  lazy val iri = IRI.create(s"c_${toString(axiom)}")
}
