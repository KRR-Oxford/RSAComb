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

package uk.ac.ox.cs.rsacomb.filtering

import tech.oxfordsemantic.jrdfox.logic.datalog.Rule
import tech.oxfordsemantic.jrdfox.logic.expression.IRI
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.util.Versioned

/** Type of filtering strategy.
  *
  * Mainly for testing different approaches and techniques.
  */
sealed trait FilterType
object FilterType {
  case object NAIVE extends FilterType
  case object REVISED extends FilterType
}

/** Filtering program trait */
object FilteringProgram extends Versioned[FilterType] {

  import FilterType._

  type Result = (IRI, IRI, ConjunctiveQuery) => FilteringProgram

  /** Returns the right type of filtering program builder.
    *
    * @param filter type of filtering program.
    * @param source source named graph for the filtering program.
    * @param target target named graph for the filtering program.
    *
    * @return the right type of filtering program builder.
    */
  def apply(filter: FilterType): Result =
    filter match {
      case NAIVE   => NaiveFilteringProgram(_, _, _)
      case REVISED => RevisedFilteringProgram(_, _, _)
    }
}

/** Filtering Program generator
  *
  * Handles the conversion of a CQ into a set of logic rules,
  * representing the filtering step of the RSA combined approach.
  */
trait FilteringProgram {

  /** Source named graph for the filtering process */
  val source: IRI

  /** Target named graph for the filtering process */
  val target: IRI

  /** Query from which the filtering program is generated */
  val query: ConjunctiveQuery

  /** Collection of filtering program rules. */
  def rules: List[Rule]

  /** Query to be used to retrieve the answers */
  def answerQuery: String

  /** Pretty-print filtering rule */
  override def toString(): String = rules mkString "\n"
}
