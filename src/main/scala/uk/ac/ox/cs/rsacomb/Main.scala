package uk.ac.ox.cs.rsacomb

/* Java imports */
import java.io.File
import java.util.HashMap
import scala.collection.JavaConverters._

import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}

/* Local imports */
import util.{Logger, RDFoxUtil, RSA}
import sparql.ConjunctiveQuery

object RSAComb extends App {

  val help: String = """
  rsacomb - combined approach for CQ answering for RSA ontologies.

  USAGE
    rsacomb <query> <ontology> ...

  where
    - query: a (single) SPARQL query file.
    - ontology: one or more ontologies.

  """

  /* Simple arguments handling
   *
   * TODO: use something better later on
   */

  if (args.length < 2) {
    println(help)
    sys.exit;
  }

  val queryPath = new File(args(0))
  val ontoPaths = args.drop(1).map(new File(_))

  if (!queryPath.isFile || !ontoPaths.forall(_.isFile)) {
    println("The provided arguments are not regular files.\n\n")
    println(help)
    sys.exit;
  }

  /* TODO: It might be required to check if the ontology in input is
   * Horn-ALCHOIQ. At the moment we are assuming this is always the
   * case.
   */

  val ontology = RSAOntology(ontoPaths: _*)
  if (ontology.isRSA) {

    Logger print "Ontology is RSA!"

    /** Read SPARQL query from file */
    val query = RDFoxUtil.loadQueryFromFile(queryPath.getAbsoluteFile)

    /* Compute answers to query */
    ConjunctiveQuery(query).map(ontology ask _) match {
      case Some(answers) => Logger print answers
      case None =>
        throw new RuntimeException("Submitted query is not conjunctive")
    }
  }
}
