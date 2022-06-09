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

package uk.ac.ox.cs.rsacomb.suffix

// import org.semanticweb.owlapi.model.{
//   OWLPropertyExpression,
//   OWLObjectInverseOf,
//   OWLObjectProperty
// }

import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}
import tech.oxfordsemantic.jrdfox.logic.datalog.{TupleTableAtom, TupleTableName}

object RSASuffix {

  def apply(suffix: String => String): RSASuffix = new RSASuffix(suffix)

}

class RSASuffix(val suffix: String => String) {

  def +(that: RSASuffix): RSASuffix =
    new RSASuffix(this.suffix andThen that.suffix)

  def ::(str: String): String = this suffix str
  def ::(iri: IRI): IRI = IRI.create(this suffix iri.getIRI)
  def ::(tta: TupleTableAtom): TupleTableAtom = {
    val ttn: TupleTableName = tta.getTupleTableName
    val args = tta.getArguments
    (args.get(1), args.get(2)) match {
      case (IRI.RDF_TYPE, obj: IRI) =>
        TupleTableAtom.create(ttn, args.get(0), IRI.RDF_TYPE, obj :: this)
      case (pred: IRI, obj: Term) =>
        TupleTableAtom.create(ttn, args.get(0), pred :: this, obj)
      case _ => tta 
    }
  }
}

case object Empty extends RSASuffix(identity)
case object Forward extends RSASuffix((s) => s"${s}_f")
case object Backward extends RSASuffix((s) => s"${s}_b")
case object Inverse extends RSASuffix((s) => s"${s}_inv")
case class Nth(n: Int) extends RSASuffix((s) => s"${s}_$n")
