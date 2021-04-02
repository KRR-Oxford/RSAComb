package uk.ac.ox.cs.rsacomb

import java.io.File
import java.util.HashMap
import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

import util.{Logger, RDFoxUtil, RSA}
import sparql.ConjunctiveQuery

case class RSAOption[+T](opt: T) {
  def get[T]: T = opt.asInstanceOf[T]
}

object RSAConfig {
  type Config = Map[Symbol, RSAOption[Any]]

  private implicit def toRSAOption[T](opt: T) = RSAOption[T](opt)

  /** Help message */
  private val help: String = """
  rsacomb - combined approach for CQ answering for RSA ontologies.

  USAGE
    rsacomb [OPTIONS] <ontology> [<data> ...]

      -h | -? | --help
          print this help message

      --rsacheck-only
          only perform the RSA check without performing any query answering.

      -q <file> | --query <file>
          path to a file containing a single SPARQL query

      <ontology>
          file containing the ontology

      <data>
          one or more data files

  """

  /** Default config values */
  private val default = Map(
    'rsacheckonly -> RSAOption[Boolean](false)
  )

  /** Utility to exit the program with a custom message on stderr.
    *
    * The program will exit with error code 1 after printing the help
    * message.
    *
    * @param msg message printed to stderr.
    */
  private def exit(msg: String): Nothing = {
    System.err.println(msg)
    System.err.println()
    System.err.println(help)
    sys.exit(1)
  }

  /** Parse arguments with default options
    *
    * @param args arguments list
    */
  def parse(args: List[String]): Config = parse(args, default)

  /** Parse arguments
    *
    * @param args arguments list
    * @param config default configuration
    */
  def parse(args: List[String], config: Config): Config = {
    args match {
      case flag @ ("-h" | "-?" | "--help") :: _ => {
        println(help)
        sys.exit(0)
      }
      case "--rsacheck-only" :: tail =>
        parse(tail, config ++ Map('rsacheckonly -> true))
      case flag @ ("-q" | "--query") :: _query :: tail => {
        val query = new File(_query)
        if (!query.isFile)
          exit(s"'$query' is not a valid filename.")
        parse(tail, config ++ Map('query -> query))
      }
      case _ontology :: _data => {
        val ontology = new File(_ontology)
        val data = _data.map(new File(_))
        (ontology :: data) foreach { (file) =>
          if (!file.isFile)
            exit(s"'$file' is not a valid filename.")
        }
        finalise(config ++ Map('ontology -> ontology, 'data -> data))
      }
      case a => exit(s"Invalid sequence of arguments '${a.mkString(" ")}'.")
    }
  }

  /** Perform final checks on parsed options */
  private def finalise(config: Config): Config = {
    // Query file is mandatory unless only the RSA check is required.
    if (!config('rsacheckonly).get[Boolean] && !config.contains('query))
      exit(s"Query file was not provided.")

    config
  }
}

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

  val config = RSAConfig.parse(args.toList)

  val ontology =
    RSAOntology(config('ontology).get[File], config('data).get[List[File]]: _*)

  if (ontology.isRSA) {

    Logger print "Ontology is RSA!"

    if (!config('rsacheckonly).get[Boolean]) {
      val query =
        RDFoxUtil.loadQueryFromFile(config('query).get[File].getAbsoluteFile)

      ConjunctiveQuery.parse(query) match {
        case Some(query) => {
          val answers = ontology ask query
          //Logger.print(s"$answers", Logger.QUIET)
          Logger print s"Number of answers: ${answers.length} (${answers.lengthWithMultiplicity})"

          //    /* Additional DEBUG information */
          //    if (Logger.level >= Logger.DEBUG) {
          //      /* Unfiltered rules */
          //      val unfiltered = ontology askUnfiltered query
          //      unfiltered map { u =>
          //        Logger print s"Number of unfiltered answers: ${u.length} (${u.map(_._1).sum})."

          //        /* Spurious answers */
          //        val spurious = {
          //          val variables = query.variables.length
          //          val sp = RDFoxUtil.buildDescriptionQuery("SP", variables)
          //          ontology.queryDataStore(query, sp, RSA.Prefixes)
          //        }
          //        spurious map { s =>
          //          Logger print s"Number of spurious answers: ${s.length} (${s.map(_._1).sum})"

          //          /* Spurious/unfiltered percentage */
          //          val perc =
          //            if (u.length > 0) (s.length / u.length.toFloat) * 100 else 0
          //          Logger print s"Percentage of spurious answers: $perc%"
          //        }
          //      }
          //    }
        }
        case None =>
          throw new RuntimeException("Submitted query is not conjunctive")
      }
    }

  } else {

    Logger print "Ontology is not RSA!"

  }
}
