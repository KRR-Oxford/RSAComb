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

package uk.ac.ox.cs.rsacomb.sparql

import ujson._
import tech.oxfordsemantic.jrdfox.logic.expression.{
  IRI,
  Literal,
  Resource,
  Variable
}

/** A collections of answers to a query.
  *
  * Will take the query type (CQ or BCQ) into consideration when
  * serialised.
  *
  * @param bcq whether the answers are the result of a BCQ.
  * @param answers collection of answers to a query. When dealing with
  * BCQs, and empty collection represents a ''false'', ''true'' otherwise.
  */
class ConjunctiveQueryAnswers(
    val query: ConjunctiveQuery,
    val variables: Seq[Variable],
    val answers: Seq[(Long, Seq[Resource])]
) {

  /** Returns number of distinct answers. */
  val length: Int = if (query.bcq) 0 else answers.length

  /** Returns number of answers taking into account multiplicity. */
  val lengthWithMultiplicity: Long = answers.map(_._1).sum

  /** Serialise answers as JSON file */
  def toJSON(): ujson.Js.Value = {
    ujson.Obj(
      "queryID" -> query.id,
      "queryText" -> query.toString
        .split('\n')
        .map(_.trim.filter(_ >= ' '))
        .mkString(" "),
      "answerVariables" -> ujson.Arr(query.answer.map(_.toString())),
      "answers" -> ujson.Arr(answers.map(_._2.mkString(" ")).sorted)
    )
  }

  override def toString(): String =
    if (query.bcq) {
      if (answers.isEmpty) "FALSE" else "TRUE"
    } else {
      if (answers.isEmpty)
        "NO ANSWER."
      else {
        val header = variables map (_.getName) mkString "\t"
        val body = answers
          .map(
            _._2
              .map {
                case x: IRI     => x.getIRI
                case x: Literal => x.getLexicalForm
                case x          => x.toString
              }
              .mkString("\t")
          )
          .mkString("\n")
        s"$header\n$body"
      }
    }
}
