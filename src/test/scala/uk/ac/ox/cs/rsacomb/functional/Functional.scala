package uk.ac.ox.cs.rsacomb.functional

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import uk.ac.ox.cs.rsacomb.ontology.Ontology
import uk.ac.ox.cs.rsacomb.approximation.Upperbound
import uk.ac.ox.cs.rsacomb.converter.Normalizer
import uk.ac.ox.cs.rsacomb.util.{Logger, RDFoxUtil}

class LUBM extends AnyFunSpec with Matchers {

  Logger.level = Logger.QUIET

  private val test = os.pwd / "tests" / "lubm"

  /* Approximation algorithms */
  //private val toLowerbound = new Lowerbound
  private val toUpperbound = new Upperbound

  /* Normalization algorithms */
  private val normalizer = new Normalizer

  /* Ontology */
  private val ontology = Ontology(
    test / "univ-bench.owl",
    List(test / "data" / "lubm1.ttl")
  ) normalize normalizer
  private val rsa = ontology approximate toUpperbound

  /* Queries and results */
  private val results = ujson.read(os.read(test / "results.json")).arr

  describe("Ontology size: 1)") {

    val queries = RDFoxUtil.loadQueriesFromFile(test / "queries.sparql")
    queries foreach { query =>
      it(s"Tested Query${query.id}") {
        val answers = rsa.ask(query).answers.map(_._2.mkString("\t"))
        val reference = results
          .find(_("queryID").num == query.id)
          .get("answers")
          .arr
          .map(_.str)
        answers should contain theSameElementsAs reference
      }
    }

    val slow = RDFoxUtil.loadQueriesFromFile(test / "queries-slow.sparql")
    slow foreach { query =>
      it(s"Tested Query${query.id}", Slow) {
        val answers = rsa.ask(query).answers.map(_._2.mkString("\t"))
        val reference = results
          .find(_("queryID").num == query.id)
          .get("answers")
          .arr
          .map(_.str)
        answers should contain theSameElementsAs reference
      }
    }

  }
}
