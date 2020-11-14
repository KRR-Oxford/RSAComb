package rsacomb

/* Java imports */
import java.io.File
import java.util.HashMap
import scala.collection.JavaConverters._

import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}

/* Local imports */
import rsacomb.RSA._

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

  /* Create RSA object from generic OWLOntology
   *
   * TODO: It might be required to check if the ontology in input is
   * Horn-ALCHOIQ. At the moment we are assuming this is always the
   * case.
   */

  val ontology: RSAOntology = RSA.loadOntology(ontoPath)
  if (ontology.isRSA) {

    /* Load query */
    val query = RDFoxUtil.parseQuery(
      """
        SELECT ?X
        WHERE {
          ?X a  :D ;
             :R ?Y .
          ?Y :S ?Z .
          ?Z a  :D .
        }
      """
    )

    /* Compute answers to query */
    query match {
      case Some(query) => {
        // Open connection to RDFox
        val (server, data) = RDFoxUtil.openConnection("AnswerComputation")

        {
          println("\nQuery")
          println(query)
        }

        // Step 1. Computing the canonical model
        val canon = ontology.canonicalModel
        data.addRules(canon.rules.asJava)

        {
          println("\nCanonical Model rules:")
          canon.rules.foreach(println)
        }

        // Step 2. Computing the canonical model
        val nis = {
          val query =
            "SELECT ?Y WHERE { ?X internal:EquivTo ?Y ; a internal:NAMED . }"
          val cursor =
            data.createCursor(
              RSA.Prefixes,
              query,
              new HashMap[String, String]()
            );
          var mul = cursor.open()
          var iris: List[IRI] = List()
          while (mul > 0) {
            println(cursor.getResource(0))
            iris = cursor.getResource(0) match {
              case iri: IRI => iri :: iris
              case _        => iris
            }
            mul = cursor.advance()
          }
          iris
        }
        val filter = ontology.filteringProgram(query, nis)
        data.addRules(filter.rules.asJava)

        {
          println("\nFiltering rules")
          filter.rules.foreach(println)
        }

        def retrieveInstances(pred: String, arity: Int): Unit = {
          // Build query
          var query = "SELECT"
          for (i <- 0 until arity) {
            query ++= s" ?X$i"
          }
          query ++= " WHERE {"
          for (i <- 0 until arity) {
            query ++= s" ?S internal:${pred}_$i ?X$i ."
          }
          query ++= " }"
          // Collect answers
          RDFoxUtil.submitQuery(
            data,
            RSA.Prefixes,
            query,
            arity
          )
        }

        // Retrieve answers
        println("\nAnswers:")
        retrieveInstances("ANS", filter.answer.length)

        /* DEBUG: adding additional checks
         */
        println("\nIndividuals:")
        ontology.individuals.foreach(println)

        println("\nThings:")
        RDFoxUtil.submitQuery(
          data,
          RSA.Prefixes,
          "SELECT ?X { ?X a owl:Thing }",
          1
        )

        println("\nNAMEDs:")
        RDFoxUtil.submitQuery(
          data,
          RSA.Prefixes,
          "SELECT ?X { ?X a internal:NAMED }",
          1
        )

        println("\nNIs:")
        RDFoxUtil.submitQuery(
          data,
          RSA.Prefixes,
          "SELECT ?X { ?X a internal:NI }",
          1
        )

        // ID instances
        println("\nID instances:")
        retrieveInstances("ID", filter.variables.length + 2)

        println("\nSameAs instances:")
        RDFoxUtil.submitQuery(
          data,
          RSA.Prefixes,
          "SELECT ?X ?Y { ?X internal:EquivTo ?Y }",
          2
        )

        // Unfiltered answers
        println("\nPossible answers:")
        retrieveInstances("QM", filter.variables.length)

        // Cycle detected
        println("\nCycle detection:")
        retrieveInstances("AQ_f", filter.variables.length + 2)
        retrieveInstances("AQ_b", filter.variables.length + 2)

        // Forks detected
        println("\nForks:")
        retrieveInstances("FK", filter.variables.length)

        // Spurious answers
        println("\nSpurious answers")
        retrieveInstances("SP", filter.variables.length)

        // Close connection to RDFox
        RDFoxUtil.closeConnection(server, data)
      }
      case None => {}
    }
  }
}

/* Notes:
 *
 * To establish a connection with a local RDFox instance, do the
 * following:
 *
 * ```
 * val serverConnection : ServerConnection = ConnectionFactory.newServerConnection("rdfox:local", "", "")
 * serverConnection.createDataStore("test","seq",new HashMap())
 * val dataStoreConnection : DataStoreConnection = serverConnection.newDataStoreConnection("test")
 * dataStoreConnection.importData(
 *    UpdateType.ADDITION,
 *    Prefixes.s_emptyPrefixes,
 *    new File("./path/to/file")
 * )
 * ```
 */
