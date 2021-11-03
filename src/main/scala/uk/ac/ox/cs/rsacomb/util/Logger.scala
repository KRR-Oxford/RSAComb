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

package uk.ac.ox.cs.rsacomb.util

import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.PrintStream
import uk.ac.ox.cs.rsacomb.sparql.{ConjunctiveQuery, ConjunctiveQueryAnswers}

/** Simple logger */
object Logger {

  /** Main directory for logger output for the current run */
  val dir = {
    val timestamp = (new SimpleDateFormat("yyyyMMddHHmmss")).format(
      Calendar.getInstance().getTime
    )
    os.pwd / s"rsacomb-$timestamp"
  }

  /** Output stream for the logger. */
  var output: PrintStream = System.out

  /** Path to answers output file */
  var answers: os.Path = dir / "answers.json"

  /** Logger levels (i.e., verbosity of output) */
  sealed abstract class Level(val level: Int, val name: String)
      extends Ordered[Level] {
    def compare(that: Level) = this.level - that.level
    override def toString = name
  }
  case object QUIET extends Level(0, "quiet")
  case object NORMAL extends Level(1, "normal")
  case object DEBUG extends Level(2, "debug")
  case object VERBOSE extends Level(3, "verbose")

  /** Currend logger level */
  var level: Level = DEBUG

  /** Print a line padded with logger level and timestamp.
    *
    * @param str object to be printed.
    * @param lvl minimum logger level required to print.
    */
  def print(str: Any, lvl: Level = NORMAL): Unit =
    if (lvl <= level)
      output println s"[$lvl][${Calendar.getInstance().getTime}] $str"

  /** Write provided content to file.
    *
    * @param content content to append to the file.
    * @param file name of the file to append the content to.
    * @param lvl minimum logger level required to write.
    */
  def write(content: => os.Source, file: String, lvl: Level = VERBOSE): Unit =
    if (lvl <= level)
      os.write.append(dir / file, content, createFolders = true)

  /** Write answers to queries to output file in JSON format.
    * 
    * @param ans the set of answers to be written.
    */
  def write(ans: Seq[ConjunctiveQueryAnswers]): Unit = {
    ujson.writeToOutputStream(
      ujson.Arr(ans.map(_.toJSON)),
      os.write.outputStream(answers, createFolders = true),
      indent = 2
    )
  }

  /** Timed evaluation of an expression.
    *
    * Records and outputs additional information about evaluation time.
    *
    * @param expr expression to be evaluated.
    * @param desc short description of the expression.
    * @param lvl minimum require logger level for output
    * @return the result of the evaluation.
    */
  def timed[A](expr: => A, desc: String = "", lvl: Level = NORMAL): A = {
    val t0 = System.currentTimeMillis()
    print(s"$desc (START)", lvl)
    val result = expr
    val t1 = System.currentTimeMillis()
    print(s"$desc (END): ${(t1 - t0).toFloat / 1000}", lvl)
    result
  }

  /** Generate simulation scripts for current run
    *
    * @param data data files to be imported.
    * @param queries collection of executed queries.
    * @param lvl minimum logger level required.
    */
  def generateSimulationScripts(
      data: Seq[os.Path],
      queries: Seq[ConjunctiveQuery],
      lvl: Level = VERBOSE
  ): Unit =
    if (lvl <= level) {
      /* Create script folder */
      val sim = os.rel / 'sim
      /* Generate main script */
      os.write.append(
        dir / "simulate.rdfox",
        """
echo "\n[Start endpoint]"
endpoint start

echo "\n[Create new datastore]"
dstore create rsacomb
active rsacomb
prefix rsacomb: <http://www.cs.ox.ac.uk/isg/RSAComb#>
tupletable create rsacomb:CanonicalModel type "named-graph"

echo "\n[Import data]"
""" ++
          data
            .map(d => s"""import > rsacomb:CanonicalModel \"$d\"""")
            .mkString("\n")
          ++ s"""
insert { graph rsacomb:CanonicalModel { ?x a owl:Thing } } where { graph rsacomb:CanonicalModel { ?x ?y ?z } }
insert { graph rsacomb:CanonicalModel { ?z a owl:Thing } } where { graph rsacomb:CanonicalModel { ?x ?y ?z } . filter( ?y != a ) }
import "axiomatisation.dlog"
insert { graph rsacomb:CanonicalModel { ?x a rsacomb:Named } } where { graph rsacomb:CanonicalModel { ?x a owl:Thing } }

echo "\\n[Load canonical model program]"
import "canonical_model.dlog"

exec "$sim/filter_query_$$(1).rdfox"
""",
        createFolders = true
      )
      /* Generate query scripts */
      queries.map(q => {
        val id = q.id
        os.write.append(
          dir / sim / "filter_query_all.rdfox",
          s"exec $sim/filter_query_$id.rdfox\n",
          createFolders = true
        )
        os.write.append(
          dir / sim / s"filter_query_$id.rdfox",
          s"""
echo "\\n[Load filtering program for query $id]"
tupletable create rsacomb:Filter$id type "named-graph"
import "filter_query_$id.dlog"
""",
          createFolders = true
        )
      })
    }
}
