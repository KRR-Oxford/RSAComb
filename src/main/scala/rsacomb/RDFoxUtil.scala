package rsacomb

/* Java imports */
import java.util.HashMap
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.{
  ConnectionFactory,
  ServerConnection,
  DataStoreConnection
}

object RDFoxUtil {

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
    //parameters.put("equality", "noUNA")
    server.createDataStore(dataStore, "seq", parameters)
    val data = server.newDataStoreConnection(dataStore)

    (server, data)
  }

  def query(
      data: DataStoreConnection,
      prefixes: Prefixes,
      query: String
  ): Unit = {
    println(s"\n{ $query }")
    val cursor = data.createCursor(
      prefixes,
      query,
      new HashMap[String, String]()
    );
    var mul = cursor.open()
    while (mul > 0) {
      val res0 = cursor.getResource(0)
      val res1 = cursor.getResource(1)
      println(s"Answer: $res0 $res1")
      mul = cursor.advance()
    }
    cursor.close();
  }

  def closeConnection(
      server: ServerConnection,
      data: DataStoreConnection
  ): Unit = {
    server.close();
    data.close();
  }

} // object RDFox
