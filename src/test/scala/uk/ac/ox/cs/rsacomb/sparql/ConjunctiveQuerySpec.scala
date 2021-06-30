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

import org.scalatest.{Inspectors, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tech.oxfordsemantic.jrdfox.logic.expression.Variable

object ConjunctiveQuerySpec {

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
    PREFIX  :  <http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#>
    SELECT  *
    WHERE {
      ?w    a                     :Wellbore ;
            :wellboreDocument     ?doc .
      ?doc  :hasURL               ?document_hyperlink
    }
  """

  val cq3 = """
    PREFIX  :  <http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#>
    SELECT  ?wellbore ?formation_pressure
    WHERE {
      ?w   a                      :Wellbore ;
           :name                  ?wellbore ;
           :hasFormationPressure  ?fp .
      ?fp  :valueInStandardUnit   ?formation_pressure
    }
  """

  val cq4 = """
    PREFIX  :  <http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#>
    SELECT  *
    WHERE {
      ?w            a                           :Wellbore ;
                    :hasGeochemicalMeasurement  ?measurement .
      ?measurement  :cgType                     ?cgtype ;
                    :peakName                   ?peakType ;
                    :peakHeight                 ?peak_height ;
                    :peakAmount                 ?peak_amount
    }
  """

  val cq5 = """
    PREFIX  :  <http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#>
    SELECT  ?wellbore ?unit_name ?discovery
    WHERE {
      ?w       a                     :Wellbore ;
               :name                 ?wellbore ;
               :hasWellboreInterval  ?c_int ;
               :hasWellboreInterval  ?f_int .
      ?c_int   :hasUnit              ?c_unit .
      ?c_unit  :name                 ?unit_name .
      ?f_int   a                     :FluidZone ;
               :name                 ?discovery ;
               :overlapsWellboreInterval  ?c_int
    }
  """

  val cq6 = """
    PREFIX  :  <http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#>
    SELECT DISTINCT ?wellbore ?content
    WHERE {
      ?w    a                     :Wellbore ;
            :name                 ?wellbore ;
            :hasWellboreInterval  ?int .
      ?int  a                     :FluidZone ;
            :fluidZoneContent     ?content
    }
  """

  val cq7 = """
    PREFIX  :  <http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#>
    SELECT  ?wName ?sample ?porosity ?top_depth_md ?bot_depth_md
    WHERE {
      ?w        a                     :Wellbore ;
                :name                 ?wName ;
                :hasWellboreInterval  ?z .
      ?z        :hasUnit              ?u .
      ?u        :name                 ?strat_unit_name .
      ?wellbore  :hasWellboreInterval  ?cored_int .
      ?c        :extractedFrom        ?cored_int ;
                :hasCoreSample        ?sample .
      ?sample   :hasDepth  ?sample_depth .
      ?sample_depth
                :inWellboreInterval   ?z .
      ?sample   :hasPorosity  ?p .
      ?p        :valueInStandardUnit  ?porosity .
      ?z        :hasTopDepth  ?top .
      ?top      a                     :MeasuredDepth ;
                :valueInStandardUnit  ?top_depth_md .
      ?z        :hasBottomDepth  ?bot .
      ?bot      a                     :MeasuredDepth ;
                :valueInStandardUnit  ?bot_depth_md
   }
  """

  val bcq0 = """
    PREFIX  :  <http://example.com/scrubs/casting/>
    ASK {
      :turk a        :Doctor ;
            :married ?X .
      ?X    a        :Nurse .
    } 
  """

  val queries =
    List(cq0, cq1, cq2, cq3, cq4, cq5, cq6, cq7, bcq0)

}

class ConjunctiveQuerySpec
    extends AnyFlatSpec
    with Matchers
    with Inspectors
    with OptionValues {

  import ConjunctiveQuerySpec._

  "A conjunctive query" should "result in a `ConjunctiveQuery` instance" in {
    ConjunctiveQuery.parse(cq0) shouldBe defined
  }

  "A boolean conjunctive query" should "result in a `ConjunctiveQuery` instance" in {
    ConjunctiveQuery.parse(bcq0) shouldBe defined
  }

  "A query with proper SELECT defined" should "not be a BCQ" in {
    ConjunctiveQuery.parse(cq0).value should not be 'bcq
  }

  "A query with a \"*\" SELECT" should "not be a BCQ" in {
    ConjunctiveQuery.parse(cq1).value should not be 'bcq
  }

  "An ASK query" should "not be a BCQ" in {
    ConjunctiveQuery.parse(bcq0).value shouldBe 'bcq
  }

  "Queries" should "have distinct answer and bounded variables" in {
    for (q <- queries) {
      val cq = ConjunctiveQuery.parse(q)
      forAll(cq.value.answer) { v => cq.value.bounded should not contain v }
      forAll(cq.value.bounded) { v => cq.value.answer should not contain v }
    }
  }

  "CQ0" should "have {?obj, ?pred} as bounded variables" in {
    ConjunctiveQuery.parse(cq0).value.bounded should contain theSameElementsAs
      List(
        Variable.create("Y"),
        Variable.create("Z")
      )
  }

  "CQ1" should "have no bounded variable" in {
    ConjunctiveQuery.parse(cq1).value.bounded shouldBe empty
  }

  "CQ2" should "have no bounded variable" in {
    ConjunctiveQuery.parse(cq2).value.bounded shouldBe empty
  }

  "CQ3" should "have {?w, ?fp} as bounded variables" in {
    ConjunctiveQuery.parse(cq3).value.bounded should contain theSameElementsAs
      List(
        Variable.create("w"),
        Variable.create("fp")
      )
  }

  "CQ4" should "have no bounded variable" in {
    ConjunctiveQuery.parse(cq4).value.bounded shouldBe empty
  }

  "CQ5" should "have a non-empty bounded set" in {
    ConjunctiveQuery.parse(cq5).value.bounded should contain theSameElementsAs
      List(
        Variable.create("w"),
        Variable.create("c_int"),
        Variable.create("f_int"),
        Variable.create("c_unit")
      )
  }

  "CQ6" should "have a non-empty bounded set" in {
    ConjunctiveQuery.parse(cq6).value.bounded should contain theSameElementsAs
      List(
        Variable.create("w"),
        Variable.create("int")
      )
  }

  "CQ7" should "have a non-empty bounded set" in {
    ConjunctiveQuery.parse(cq7).value.bounded should contain theSameElementsAs
      List(
        Variable.create("w"),
        Variable.create("z"),
        Variable.create("u"),
        Variable.create("strat_unit_name"),
        Variable.create("wellbore"),
        Variable.create("cored_int"),
        Variable.create("c"),
        Variable.create("sample_depth"),
        Variable.create("p"),
        Variable.create("top"),
        Variable.create("bot")
      )
  }

}
