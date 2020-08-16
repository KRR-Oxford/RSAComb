package rsacomb

/* Java imports */
import java.util.HashMap
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

  def closeConnection(
      server: ServerConnection,
      data: DataStoreConnection
  ): Unit = {
    server.close();
    data.close();
  }

} // object RDFox
