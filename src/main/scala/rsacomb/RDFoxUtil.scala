package rsacomb

/* Java imports */
import java.util.HashMap
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.IRI
import tech.oxfordsemantic.jrdfox.client.{
  ConnectionFactory,
  ServerConnection,
  DataStoreConnection
}
object RDFoxUtil {

  implicit def owlapi2rdfox(iri: org.semanticweb.owlapi.model.IRI): IRI = {
    IRI.create(iri.getIRIString())
  }

  implicit def owlapi2rdfox(iri: String): IRI = {
    IRI.create(iri)
  }

  def openConnection(
      dataStore: String
  ): (ServerConnection, DataStoreConnection) = {
    /* Create local server connection
     */
    val serverUrl = "rdfox:local"
    val role = ""
    val password = ""
    val server =
      ConnectionFactory.newServerConnection(serverUrl, role, password)

    /* Create datastore connection
     */
    val parameters = new HashMap[String, String]()
    parameters.put("owl-in-rdf-support", "relaxed")
    //parameters.put("equality", "noUNA")
    server.createDataStore(dataStore, "par-complex-nn", parameters)
    val data = server.newDataStoreConnection(dataStore)

    (server, data)
  }

  def submitQuery(
      data: DataStoreConnection,
      prefixes: Prefixes,
      query: String,
      answers: Int
  ): Unit = {
    println(s"\nQUERY { $query }")
    val cursor = data.createCursor(
      prefixes,
      query,
      new HashMap[String, String]()
    );
    var mul = cursor.open()
    while (mul > 0) {
      print("Answer: ")
      for (i <- 0 until answers) {
        val res = cursor.getResource(i)
        print(s"$res ")
      }
      println()
      mul = cursor.advance()
    }
    cursor.close();
    println(s"QUERY END")
  }

  def closeConnection(
      server: ServerConnection,
      data: DataStoreConnection
  ): Unit = {
    server.close();
    data.close();
  }

} // object RDFoxUtil
