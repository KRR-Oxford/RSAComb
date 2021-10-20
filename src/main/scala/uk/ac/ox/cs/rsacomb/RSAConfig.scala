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

import scala.collection.mutable.Map
import util.Logger

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

      -l | --logger <level>
          specify the logger verbosity. Values are: quiet, normal (default),
          debug, verbose.

      -a | --answers <file>
        path to the output file for the answers to the query (in JSON
        format)

      -q | --queries <file>
          path to a file containing a single SPARQL query. If no query
          is provided, only the approximation to RSA will be performed.

      -o | --ontology <file>
          ontology file in OWL format.

      -d | --data <file>
          data files to be used alongside the ontology file. If a
          directory is provided, all files in the directory (recursively)
          will be considered.

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

  private def getPath(str: String): os.Path =
    try {
      os.Path(str, base = os.pwd)
    } catch {
      case e: IllegalArgumentException =>
        exit(s"'$str' is not a well formed path.")
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
      case Nil => finalise(config)
      case flag @ ("-h" | "-?" | "--help") :: _ => {
        println(help)
        sys.exit(0)
      }
      case flag @ ("-l" | "--logger") :: _level :: tail => {
        val level = _level match {
          case "quiet"   => Logger.QUIET
          case "debug"   => Logger.DEBUG
          case "verbose" => Logger.VERBOSE
          case _         => Logger.NORMAL
        }
        parse(tail, config += ('logger -> level))
      }
      case flag @ ("-a" | "--answers") :: answers :: tail =>
        parse(tail, config += ('answers -> getPath(answers)))
      case flag @ ("-q" | "--queries") :: _query :: tail => {
        val query = getPath(_query)
        if (!os.isFile(query))
          exit(s"'${_query}' is not a valid filename.")
        parse(tail, config += ('queries -> query))
      }
      case flag @ ("-o" | "--ontology") :: _ontology :: tail => {
        val ontology = getPath(_ontology)
        if (!os.isFile(ontology))
          exit(s"'${_ontology}' is not a valid filename.")
        parse(tail, config += ('ontology -> ontology))
      }
      case flag @ ("-d" | "--data") :: _data :: tail => {
        val data = getPath(_data)
        val files =
          if (os.isFile(data))
            Seq(data)
          else if (os.isDir(data))
            os.walk(data).filter(os.isFile)
          else
            exit(s"'${_data}' is not a valid path.")
        parse(tail, config += ('data -> files))
      }
      case a => exit(s"Invalid sequence of arguments '${a.mkString(" ")}'.")
    }
  }

  /** Perform final checks on parsed options */
  private def finalise(config: Config): Config = {
    if (!config.contains('ontology))
      exit("The following flag is mandatory: '-o' or '--ontology'.")
    if (!config.contains('data))
      config += ('data -> List.empty[os.Path])
    if (!config.contains('answers))
      config += ('answers -> Logger.dir / "answers.json")
    config
  }
}
