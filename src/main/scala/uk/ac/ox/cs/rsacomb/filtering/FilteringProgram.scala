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
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.util.Versioned

sealed trait FilterType
object FilterType {
  case object NAIVE extends FilterType
  case object REVISED extends FilterType
}

object FilteringProgram extends Versioned[FilterType] {

  import FilterType._

  type Result = (ConjunctiveQuery) => FilteringProgram

  def apply(t: FilterType): (ConjunctiveQuery) => FilteringProgram =
    t match {
      case NAIVE   => NaiveFilteringProgram(_)
      case REVISED => RevisedFilteringProgram(_)
    }
}

/** Filtering Program generator
  *
  * Handles the conversion of a CQ into a set of logic rules,
  * representing the filtering step of the RSA combined approach.
  */
trait FilteringProgram {

  /** Query from which the filtering program is generated */
  val query: ConjunctiveQuery

  /** Collection of filtering program rules. */
  def rules: List[Rule]

  /** Query to be used to retrieve the answers */
  def answerQuery: String

  /** Pretty-print filtering rule */
  override def toString(): String = rules mkString "\n"
}
