package uk.ac.ox.cs.rsacomb

/* Java imports */
import java.io.File
import java.util.HashMap
import scala.collection.JavaConverters._

import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}

/* Local imports */
import util.{RDFoxUtil, RSA}
import sparql.ConjunctiveQuery

object RSAComb extends App {

  val help: String = """
  rsacomb - combined approach for CQ answering for RSA ontologies.

  USAGE
    rsacomb <path/to/ontology.owl> <path/to/query.sparql>

  where
    the ontology is expected to be an OWL file and the (single)
    query a SPARQL query file.

  """

  /* Simple arguments handling
   *
   * TODO: use something better later on
   */

  if (args.length < 2) {
    println(help)
    sys.exit;
  }

  val ontoPath = new File(args(0))
  val queryPath = new File(args(1))

  if (!ontoPath.isFile || !queryPath.isFile) {
    println("The provided arguments are not regular files.\n\n")
    println(help)
    sys.exit;
  }

  /* TODO: It might be required to check if the ontology in input is
   * Horn-ALCHOIQ. At the moment we are assuming this is always the
   * case.
   */

  val ontology = RSAOntology(ontoPath)
  if (ontology.isRSA) {

    /** Read SPARQL query from file */
    val source = io.Source.fromFile(queryPath.getAbsoluteFile)
    val query = source.getLines mkString "\n"
    source.close()

    /* Compute answers to query */
    val answers = ConjunctiveQuery(query).map(ontology ask _)
    answers map (_.toString) foreach println
  }
}
