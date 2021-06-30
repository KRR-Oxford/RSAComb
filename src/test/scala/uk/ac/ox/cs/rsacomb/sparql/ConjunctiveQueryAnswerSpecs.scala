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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Variable}

object ConjunctiveQueryAnswerSpec {

  val varX = Variable.create("X")
  val varY = Variable.create("Y")
  val varZ = Variable.create("Z")
  val iri1 = IRI.create("_:iri1")
  val iri2 = IRI.create("_:iri2")
  val iri3 = IRI.create("_:iri3")

  val oneAnswer = new ConjunctiveQueryAnswers(
    false,
    Seq(varX, varY, varZ),
    Seq((4, Seq(iri1, iri2, iri3)))
  )
  val multipleAnswers =
    new ConjunctiveQueryAnswers(
      false,
      Seq(varY, varZ),
      Seq((1, Seq(iri1, iri1)), (2, Seq(iri1, iri2)), (1, Seq(iri1, iri3)))
    )
  val noAnswer = new ConjunctiveQueryAnswers(false, Seq(), Seq())
  val emptyAnswer =
    new ConjunctiveQueryAnswers(false, Seq(varX, varY), Seq((3, Seq())))

  val falseAnswer = new ConjunctiveQueryAnswers(true, Seq(), Seq())
  val trueAnswer1 = new ConjunctiveQueryAnswers(true, Seq(), Seq((1, Seq())))
  val trueAnswer2 =
    new ConjunctiveQueryAnswers(
      true,
      Seq(varX, varY),
      Seq((5, Seq(iri1, iri1)), (2, Seq(iri1, iri2)), (1, Seq(iri1, iri3)))
    )
}

class ConjunctiveQueryAnswerSpec extends AnyFlatSpec with Matchers {

  import ConjunctiveQueryAnswerSpec._

  "Test answer 1" should "have length 1 (4 with multiplicity)" in {
    oneAnswer should have(
      'length (1),
      'lengthWithMultiplicity (4)
    )
  }
  "Test answer 2" should "have length 3 (4 with multiplicity)" in {
    multipleAnswers should have(
      'length (3),
      'lengthWithMultiplicity (4)
    )
  }
  "Test answer 3" should "have length 0 (0 with multiplicity)" in {
    noAnswer should have(
      'length (0),
      'lengthWithMultiplicity (0)
    )
  }
  "Test answer 4" should "have length 1 (3 with multiplicity)" in {
    emptyAnswer should have(
      'length (1),
      'lengthWithMultiplicity (3)
    )
  }
  "Test boolean answer 1" should "have length 0 (0 with multiplicity)" in {
    falseAnswer should have(
      'length (0),
      'lengthWithMultiplicity (0)
    )
  }
  "Test boolean answer 2" should "have length 1 (1 with multiplicity)" in {
    trueAnswer1 should have(
      'length (0),
      'lengthWithMultiplicity (1)
    )
  }
  "Test boolean answer 3" should "have length 3 (8 with multiplicity)" in {
    trueAnswer2 should have(
      'length (0),
      'lengthWithMultiplicity (8)
    )
  }

  "A conjunctive query" should "print an header and a single line if it has a single answer" in {
    oneAnswer.toString shouldBe s"X\tY\tZ\n${iri1.getIRI}\t${iri2.getIRI}\t${iri3.getIRI}"
  }

  it should "print a header and multiple answers on multiple lines" in {
    multipleAnswers.toString shouldBe s"Y\tZ\n${iri1.getIRI}\t${iri1.getIRI}\n${iri1.getIRI}\t${iri2.getIRI}\n${iri1.getIRI}\t${iri3.getIRI}"
  }

  it should "print a special \"NO ANSWER.\" string when it has no answer" in {
    noAnswer.toString shouldBe "NO ANSWER."
  }

  it should "print only the header when it has an empty answer" in {
    emptyAnswer.toString shouldBe "X\tY\n"
  }

  "A boolean conjunctive query" should "print \"FALSE\" when it has no answer" in {
    falseAnswer.toString shouldBe "FALSE"
  }

  it should "print \"TRUE\" when it has a single empty answer" in {
    trueAnswer1.toString shouldBe "TRUE"
  }

  it should "print \"TRUE\" when it has a non-empty collection of answers" in {
    trueAnswer2.toString shouldBe "TRUE"
  }
}
