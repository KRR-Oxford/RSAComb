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
import java.io.PrintStream

/** Rough implementation of a logger.
  *
  * This is a WIP class for debugging and benchmarking.
  */
object Logger {

  private val time = Calendar.getInstance()

  private lazy val dir = os.temp.dir(os.pwd, "rsacomb-", false)

  /** Output stream for the logger. */
  var output: PrintStream = System.out

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

  def print(str: Any, lvl: Level = NORMAL): Unit =
    if (lvl <= level)
      output println s"[$lvl][${time.getTime}] $str"

  def write(content: => os.Source, file: String, lvl: Level = VERBOSE): Unit =
    if (lvl <= level)
      os.write.append(dir / file, content)

  def timed[A](expr: => A, desc: String = "", lvl: Level = NORMAL): A = {
    val t0 = System.currentTimeMillis()
    print(s"$desc (START)", lvl)
    val result = expr
    val t1 = System.currentTimeMillis()
    print(s"$desc (END): ${(t1 - t0).toFloat / 1000}", lvl)
    result
  }

}
