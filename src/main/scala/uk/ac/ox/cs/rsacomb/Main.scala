package uk.ac.ox.cs.rsacomb

import java.io.File
import java.util.HashMap
import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

import util.{Logger, RDFoxUtil, RSA}
import sparql.ConjunctiveQuery

/** Entry point of the program.
  *
  * The executable expects a SPARQL query and a non-empty sequence of
  * ontology files as arguments. The query file is expected to contain
  * exactly one query, while the ontology files will be programmatically
  * merged in a single ontology.
  *
  * @todo better arguments handling is needed. Look into some library
  * for this.
  * @todo at the moment the input ontology is assumed to be Horn-ALCHOIQ.
  * This might not be the case.
  */
object RSAComb extends App {

  val help: String = """
  rsacomb - combined approach for CQ answering for RSA ontologies.

  USAGE
    rsacomb <query> <ontology> [...]

  where
    - query: path to a file containing a single SPARQL query
    - ontology: one or more ontology files

  """

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

  val ontology = RSAOntology(ontoPaths: _*)
  if (ontology.isRSA) {

    Logger print "Ontology is RSA!"

    val query = RDFoxUtil.loadQueryFromFile(queryPath.getAbsoluteFile)

    ConjunctiveQuery.parse(query) match {
      case Some(query) => {
        val answers = ontology ask query
        Logger.print(s"$answers", Logger.QUIET)
        Logger print s"Number of answer: ${answers.length} (${answers.lengthWithMultiplicity})"

        /* Additional DEBUG information */
        if (Logger.level >= Logger.DEBUG) {
          /* Unfiltered rules */
          val unfiltered = ontology askUnfiltered query
          unfiltered map { u =>
            Logger print s"Number of unfiltered answers: ${u.length} (${u.map(_._1).sum})."

            /* Spurious answers */
            val spurious = {
              val variables = query.variables.length
              val sp = RDFoxUtil.buildDescriptionQuery("SP", variables)
              ontology.queryDataStore(query, sp, RSA.Prefixes)
            }
            spurious map { s =>
              Logger print s"Number of spurious answers: ${s.length} (${s.map(_._1).sum})"

              /* Spurious/unfiltered percentage */
              val perc =
                if (u.length > 0) (s.length / u.length.toFloat) * 100 else 0
              Logger print s"Percentage of spurious answers: $perc%"
            }
          }
        }
      }
      case None =>
        throw new RuntimeException("Submitted query is not conjunctive")
    }

  }
}
