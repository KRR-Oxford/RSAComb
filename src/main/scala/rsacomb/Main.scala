package rsacomb

/* Java imports */
import java.io.File
import java.util.HashMap
import scala.collection.JavaConverters._

import tech.oxfordsemantic.jrdfox.client.UpdateType

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
        SELECT ?uno
        WHERE {
          ?uno a  :D ;
               :R ?due .
          ?due :S ?tre .
          ?tre a  :D .
        }
      """
    )

    /* Compute answers to query */
    query match {
      case Some(query) => {
        // Open connection to RDFox
        val (server, data) = RDFoxUtil.openConnection("AnswerComputation")

        // Gather canonical model and filtering rules
        val canon = ontology.canonicalModel
        val filter = ontology.filteringProgram(query)

        // Import relevant data
        data.importData(UpdateType.ADDITION, RSA.Prefixes, ":a a :A .")
        data.addRules(canon.rules.asJava)
        data.addRules(filter.rules.asJava)

        // Collect answers to query
        for ((v, i) <- filter.variables.view.zipWithIndex) {
          println(s"Variable $i:")
          val query = s"SELECT ?X ?Y WHERE { ?X internal:Ans_$i ?Y }"
          val cursor =
            data.createCursor(
              RSA.Prefixes,
              query,
              new HashMap[String, String]()
            );
          var mul = cursor.open()
          while (mul > 0) {
            printf(
              "Ans_%d(%s,%s)",
              i,
              cursor.getResource(0),
              cursor.getResource(1)
            )
            mul = cursor.advance()
          }
        }

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
