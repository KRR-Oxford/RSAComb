package uk.ac.ox.cs.rsacomb.sparql

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tech.oxfordsemantic.jrdfox.logic.expression.IRI

object ConjunctiveQueryAnswerSpec {

  val iri1 = IRI.create("_:iri1")
  val iri2 = IRI.create("_:iri2")
  val iri3 = IRI.create("_:iri3")

  val oneAnswer = new ConjunctiveQueryAnswers(false, Seq(Seq(iri1, iri2, iri3)))
  val multipleAnswers =
    new ConjunctiveQueryAnswers(
      false,
      Seq(Seq(iri1, iri1), Seq(iri1, iri2), Seq(iri1, iri3))
    )
  val noAnswer = new ConjunctiveQueryAnswers(false, Seq())
  val emptyAnswer = new ConjunctiveQueryAnswers(false, Seq(Seq()))

  val falseAnswer = new ConjunctiveQueryAnswers(true, Seq())
  val trueAnswer1 = new ConjunctiveQueryAnswers(true, Seq(Seq()))
  val trueAnswer2 =
    new ConjunctiveQueryAnswers(
      true,
      Seq(Seq(iri1, iri1), Seq(iri1, iri2), Seq(iri1, iri3))
    )
}

class ConjunctiveQueryAnswerSpec extends AnyFlatSpec with Matchers {

  import ConjunctiveQueryAnswerSpec._

  "A conjunctive query" should "print a single line if it has a single answer" in {
    oneAnswer.toString shouldBe s"($iri1, $iri2, $iri3)"
  }

  it should "print multiple answers on multiple lines" in {
    multipleAnswers.toString shouldBe s"($iri1, $iri1)\n($iri1, $iri2)\n($iri1, $iri3)"
  }

  it should "print a special \"NO ANSWER\" string when it has no answer" in {
    noAnswer.toString shouldBe "NO ANSWER"
  }

  it should "print an empty list when it has an empty answer" in {
    emptyAnswer.toString shouldBe "()"
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
