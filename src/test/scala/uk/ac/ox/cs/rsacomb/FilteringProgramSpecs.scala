package rsacomb

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tech.oxfordsemantic.jrdfox.logic.expression.IRI
import uk.ac.ox.cs.rsacomb.FilteringProgram
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery

object FilteringProgramSpec {

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

class FilteringProgramSpec extends AnyFlatSpec with Matchers {

  import FilteringProgramSpec._

  "CQ 0" should "generate 27 rules and 3 facts" in {
    val cq = ConjunctiveQuery(cq0).get
    val filter = FilteringProgram(cq, constants)
    filter.facts should have length 3
    filter.rules should have length 27
  }

  "CQ 1" should "generate 15 rules" in {
    val cq = ConjunctiveQuery(cq1).get
    val filter = FilteringProgram(cq, List())
    filter.facts shouldBe empty
    filter.rules should have length 15
  }

  "CQ 2" should "generate 51 rules" in {
    val cq = ConjunctiveQuery(cq2).get
    val filter = FilteringProgram(cq, List())
    filter.facts shouldBe empty
    filter.rules should have length 51
  }

  "BCQ 0" should "generate 46 rules" in {
    val cq = ConjunctiveQuery(bcq0).get
    val filter = FilteringProgram(cq, constants)
    filter.facts should have length 3
    filter.rules should have length 43
  }

}
