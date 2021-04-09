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

      -q <file> | --query <file>
          path to a file containing a single SPARQL query. If no query
          is provided, only the approximation to RSA will be performed.

      <ontology>
          file containing the ontology

      <data>
          one or more data files

  """

  /** Default config values */
  private val default: Config = Map.empty

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
  private def finalise(config: Config): Config = config
}

/** Main entry point to the program */
object RSAComb extends App {

  /* Command-line options */
  val config = RSAConfig.parse(args.toList)

  val ontology = RSAOntology(
    config('ontology).get[File],
    config('data).get[List[File]]: _*
  )
  val rsa = ontology.toRSA()
  ontology.statistics()

  if (config contains 'query) {
    val query =
      RDFoxUtil.loadQueryFromFile(config('query).get[File].getAbsoluteFile)

    ConjunctiveQuery.parse(query) match {
      case Some(query) => {
        val answers = rsa ask query
        Logger.print(s"$answers", Logger.VERBOSE)
        Logger print s"Number of answers: ${answers.length} (${answers.lengthWithMultiplicity})"
      }
      case None =>
        throw new RuntimeException("Submitted query is not conjunctive")
    }
  }
}
