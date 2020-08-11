package rsacomb

/* Java imports */
import java.util.HashMap
import tech.oxfordsemantic.jrdfox.client.{ConnectionFactory,ServerConnection,DataStoreConnection}

object RDFox {

    def openConnection(dataStore: String): (ServerConnection,DataStoreConnection) = {
      val serverUrl = "rdfox:local"
      val role = ""
      val password = ""
      val server = ConnectionFactory.newServerConnection(serverUrl, role, password)
      server.createDataStore(dataStore,"seq",new HashMap())
      val data = server.newDataStoreConnection(dataStore)
      (server,data)
    }

    def closeConnection(server: ServerConnection, data: DataStoreConnection): Unit = {
      server.close();
      data.close();
    }

} // object RDFox