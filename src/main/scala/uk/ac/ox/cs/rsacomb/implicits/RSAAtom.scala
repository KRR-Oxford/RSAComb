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

package uk.ac.ox.cs.rsacomb.implicits

import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.expression.{Literal, FunctionCall, Term}
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  BindAtom,
  TupleTableAtom,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI}

import uk.ac.ox.cs.rsacomb.RSAUtil
import uk.ac.ox.cs.rsacomb.RSAOntology
import uk.ac.ox.cs.rsacomb.suffix.{RSASuffix, Nth}
import uk.ac.ox.cs.rsacomb.util.RDFoxUtil

/* Is this the best way to determine if an atom is an RDF triple?
 * Note that we can't use `getNumberOfArguments()` because is not
 * "consistent":
 * - for an atom created with `rdf(<term1>, <term2>, <term3>)`,
 * `getNumberOfArguments` returns 3
 * - for an atom created with `Atom.create(<tupletablename>, <term1>,
 * <term2>, <term3>)`, `getNumberOfArguments()` returns 3
 *
 * This is probably because `Atom.rdf(...) is implemented as:
 * ```scala
 *  def rdf(term1: Term, term2: Term, term3: Term): Atom =
 *    Atom.create(TupleTableName.create("rdfox:DefaultTriples"), term1, term2, term3)
 * ```
 */

object RSAAtom {

  implicit class RSAAtom(val atom: TupleTableAtom) {

    import RDFox._
    import JavaCollections._

    val name: String = atom.getTupleTableName.getName

    val args: List[Term] = atom.getArguments

    val isRDF: Boolean =
      name == "http://oxfordsemantic.tech/RDFox#DefaultTriples"

    val isClassAssertion: Boolean = {
      isRDF && {
        val pred = atom.getArguments.get(1)
        pred == IRI.RDF_TYPE
      }
    }

    val isRoleAssertion: Boolean = isRDF && !isClassAssertion

    def <<(suffix: RSASuffix): TupleTableAtom =
      if (isRDF) {
        val subj = atom.getArguments.get(0)
        val pred = atom.getArguments.get(1)
        val obj = atom.getArguments.get(2)
        if (isClassAssertion) {
          val obj1 = obj match {
            case iri: IRI => IRI.create(iri.getIRI :: suffix)
            case other    => other
          }
          TupleTableAtom.rdf(subj, pred, obj1)
        } else {
          val pred1 = pred match {
            case iri: IRI => IRI.create(iri.getIRI :: suffix)
            case other    => other
          }
          TupleTableAtom.rdf(subj, pred1, obj)
        }
      } else {
        val ttname = TupleTableName.create(name :: suffix)
        TupleTableAtom.create(ttname, atom.getArguments())
      }

    lazy val reified: (Option[TupleTableAtom], List[TupleTableAtom]) =
      if (isRDF) {
        (None, List(atom))
      } else {
        val varS = RSAUtil.genFreshVariable()
        val skolem = RDFoxUtil.skolem(name, (args :+ varS): _*)
        val atom = TupleTableAtom.rdf(varS, IRI.RDF_TYPE, name)
        val atoms = args.zipWithIndex
          .map { case (a, i) => TupleTableAtom.rdf(varS, name :: Nth(i), a) }
        (Some(skolem), atom :: atoms)
      }
  }
}
