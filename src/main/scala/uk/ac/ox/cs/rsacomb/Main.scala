package uk.ac.ox.cs.rsacomb

/* Java imports */
import java.io.File
import java.util.HashMap
import scala.collection.JavaConverters._

import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}

/* Local imports */
import util.{RDFoxHelpers, RSA}

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

  val ontology = RSAOntology(ontoPath)
  if (ontology.isRSA) {

    /* Load query */
    val query = RDFoxHelpers.parseSelectQuery(
      """
        PREFIX  :  <http://example.com/rsa_example.owl#>

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

        import implicits.JavaCollections._

        // Open connection to RDFox
        val (server, data) = RDFoxHelpers.openConnection("AnswerComputation")

        {
          println("\nQuery")
          println(query)
        }

        // Step 1. Computing the canonical model
        val canon = ontology.canonicalModel
        data.addRules(canon.rules)

        {
          println("\nCanonical Model rules:")
          canon.rules.foreach(println)
        }

        // Step 2. Computing the canonical model
        val nis = {
          val query = "SELECT ?Y WHERE { ?X rsa:EquivTo ?Y ; a rsa:Named . }"
          RDFoxHelpers.submitSelectQuery(data, query, RSA.Prefixes).flatten
        }
        val filter = ontology.filteringProgram(query, nis)
        data.addRules(filter.rules)

        {
          println("\nFiltering rules")
          filter.rules.foreach(println)
        }

        // Retrieve answers
        println("\nAnswers:")
        val ans =
          RDFoxHelpers.queryInternalPredicate(data, "Ans", filter.answer.length)
        println(ans)

        /* DEBUG: adding additional checks
         */
        {
          import suffix.{Forward, Backward}

          val arity = filter.answer.length + filter.bounded.length

          println("\nIndividuals:")
          ontology.individuals.foreach(println)

          println("\nThings:")
          val things = RDFoxHelpers.submitSelectQuery(
            data,
            """
             PREFIX  owl:  <http://www.w3.org/2002/07/owl#>

             SELECT ?X {
               ?X a owl:Thing
             }
             """
          )
          println(things)

          println("\nNAMEDs:")
          val named = RDFoxHelpers.submitSelectQuery(
            data,
            """
             SELECT ?X {
               ?X a rsa:Named
             }
             """,
            RSA.Prefixes
          )
          println(named)

          println("\nNIs:")
          val nis = RDFoxHelpers.submitSelectQuery(
            data,
            """
             SELECT ?X {
               ?X a rsa:NI
             }
             """,
            RSA.Prefixes
          )
          println(nis)

          // ID instances
          println("\nIDs:")
          val ids = RDFoxHelpers.queryInternalPredicate(
            data,
            "ID",
            arity + 2
          )
          println(ids)

          println("\nEquivTo:")
          val equivs = RDFoxHelpers.submitSelectQuery(
            data,
            """
              SELECT ?X ?Y {
                ?X rsa:EquivTo ?Y
              }
            """,
            RSA.Prefixes
          )
          println(equivs)

          // Unfiltered answers
          println("\nPossible answers:")
          val qms = RDFoxHelpers.queryInternalPredicate(
            data,
            "QM",
            arity
          )
          println(qms)

          // Cycle detected
          println("\nCycle detection:")
          val aqf = RDFoxHelpers.queryInternalPredicate(
            data,
            "AQ" :: Forward,
            arity + 2
          )
          val aqb = RDFoxHelpers.queryInternalPredicate(
            data,
            "AQ" :: Backward,
            arity + 2
          )
          println(aqf)
          println(aqb)

          // Forks detected
          println("\nForks:")
          val fk = RDFoxHelpers.queryInternalPredicate(
            data,
            "FK",
            arity
          )
          println(fk)

          // Spurious answers
          println("\nSpurious answers")
          val sp = RDFoxHelpers.queryInternalPredicate(
            data,
            "SP",
            arity
          )
          println(sp)
        }

        // Close connection to RDFox
        RDFoxHelpers.closeConnection(server, data)
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
