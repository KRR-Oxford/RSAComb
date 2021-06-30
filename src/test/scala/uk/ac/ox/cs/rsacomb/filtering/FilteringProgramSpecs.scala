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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tech.oxfordsemantic.jrdfox.logic.expression.IRI
import uk.ac.ox.cs.rsacomb.filtering.{FilteringProgram, FilterType}
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery

object NaiveFilteringProgramSpec {

  val naive: FilterType = FilterType.NAIVE

  val constants =
    List(IRI.create("_:iri1"), IRI.create("_:iri2"), IRI.create("_:iri3"))

  val cq0 = """
    PREFIX  :  <http://example.com/rsa_example.owl#>

    SELECT ?X
    WHERE {
      ?X a  :D ;
         :R ?Y .
      ?Y :S ?Z .
      ?Z a  :D .
    }
  """

  val cq1 = """
    PREFIX  :  <http://example.com/rsa_example.owl#>

    SELECT *
    WHERE {
      ?X a  :D ;
         :R ?Y .
      ?Y :S ?Z .
      ?Z a  :D .
    }
  """

  val cq2 = """
    PREFIX  :  <http://example.com/rsa_example.owl#>

    SELECT ?X
    WHERE {
      ?X a  :D ;
         :R ?Y .
      ?Y :S ?Z .
      ?Y :T ?W .
      ?Z a  :D .
      ?W a  :D
    }
  """

  val bcq0 = """
    PREFIX  :  <http://example.com/rsa_example.owl#>

    ASK {
      ?X a  :D ;
         :R ?Y .
      ?Y :S ?Z .
      ?Z a  :D .
    }
  """
}

class NaiveFilteringProgramSpec extends AnyFlatSpec with Matchers {

  import NaiveFilteringProgramSpec._

  "CQ 0" should "generate 27 rules and 3 facts" in {
    val cq = ConjunctiveQuery.parse(cq0).get
    val filter = FilteringProgram(naive)(cq)
    filter.rules should have length 27
  }

  "CQ 1" should "generate 15 rules" in {
    val cq = ConjunctiveQuery.parse(cq1).get
    val filter = FilteringProgram(naive)(cq)
    filter.rules should have length 15
  }

  "CQ 2" should "generate 51 rules" in {
    val cq = ConjunctiveQuery.parse(cq2).get
    val filter = FilteringProgram(naive)(cq)
    filter.rules should have length 51
  }

  "BCQ 0" should "generate 46 rules" in {
    val cq = ConjunctiveQuery.parse(bcq0).get
    val filter = FilteringProgram(naive)(cq)
    filter.rules should have length 43
  }

}
