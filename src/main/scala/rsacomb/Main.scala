package rsacomb

/* Java imports */
import java.io.File

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

  val ontology = RSA.loadOntology(ontoPath)
  if (ontology.isRSA) {

    /* Build canonical model */
    //val tboxCanon = rsa.canonicalModel()

    // DEBUG: print program to generate canonical model
    {
      import tech.oxfordsemantic.jrdfox.logic.{Variable}
      import org.semanticweb.owlapi.model.parameters.Imports
      import java.util.stream.{Collectors}
      import scala.collection.JavaConverters._

      val visitor = ProgramGenerator(Variable.create("x"), ontology.unsafeRoles)
      val axioms =
        ontology
          .tboxAxioms(Imports.INCLUDED)
          .collect(Collectors.toList())
          .asScala
      println("Program to generate the canonical model:")
      axioms.flatMap(_.accept(visitor)).foreach(println)
    }

    /* Load query */
    val query = RDFoxUtil.parseQuery(
      "SELECT ?X WHERE {?X ?Y ?Z}"
    )

    /* Compute the filtering program from the given query */
    val filter = ontology.filteringProgram(query)

    /* ... */
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
