/*
 * Copyright 2020, 2021 KRR Oxford
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ox.cs.rsacomb

import java.io.{File, PrintWriter}
import java.nio.file.{Path, Paths, InvalidPathException}
import java.util.HashMap
import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

import util.{Logger, RDFoxUtil, RSA}
import sparql.ConjunctiveQuery

import uk.ac.ox.cs.rsacomb.ontology.Ontology
import uk.ac.ox.cs.rsacomb.converter.Normalizer
import uk.ac.ox.cs.rsacomb.approximation.{Upperbound, Lowerbound}

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

      -q <file> | --queries <file>
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
    * @return map of config options
    */
  def parse(args: List[String]): Config = parse(args, default)

  /** Parse arguments
    *
    * @param args arguments list
    * @param config default configuration
    * @return map of config options
    */
  def parse(args: List[String], config: Config): Config = {
    args match {
      case flag @ ("-h" | "-?" | "--help") :: _ => {
        println(help)
        sys.exit(0)
      }
      case flag @ ("-o" | "--output") :: _output :: tail =>
        try {
          val output = Paths.get(_output)
          parse(tail, config ++ Map('output -> output))
        } catch {
          case e: InvalidPathException =>
            exit(s"'${_output}' is not a valid filename.")
        }
      case flag @ ("-q" | "--queries") :: _query :: tail => {
        val query = new File(_query)
        if (!query.isFile)
          exit(s"'$query' is not a valid filename.")
        parse(tail, config ++ Map('queries -> query))
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

  /* Load original ontology and normalize it */
  val ontology = Ontology(
    config('ontology).get[File],
    config('data).get[List[File]]
  ).normalize(new Normalizer)

  //ontology.axioms foreach println

  /* Approximate the ontology to RSA */
  val toRSA = new Upperbound
  val rsa = ontology approximate toRSA

  if (config contains 'queries) {
    val queries =
      RDFoxUtil.loadQueriesFromFile(config('queries).get[File].getAbsoluteFile)

    val answers = rsa ask queries

    /* Write answers to output file */
    val output = new PrintWriter(config('output).get[Path].toFile)
    output.write(ujson.write(ujson.Arr(answers.map(_.toJSON)), indent = 4))
    output.close()

    // Logger.print(s"$answers", Logger.VERBOSE)
    // Logger print s"Number of answers: ${answers.length} (${answers.lengthWithMultiplicity})"
    // Retrieve unfiltered answers
    // val unfiltered = rsa.queryDataStore(
    //   """
    //     SELECT (count(?K) as ?COUNT)
    //     WHERE {
    //         ?K a rsa:QM .
    //     }
    //   """,
    //   RSA.Prefixes
    // )
    // unfiltered.foreach((u) =>
    //   Logger print s"Number of unfiltered answers: ${u.head._2}"
    // )
  }
}
