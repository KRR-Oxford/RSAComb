package uk.ac.ox.cs.rsacomb.util

import java.util.Calendar
import java.io.PrintStream

/** Rough implementation of a logger.
  *
  * This is a WIP class for debugging and benchmarking.
  */
object Logger {

  /** Output stream for the logger. */
  var output: PrintStream = System.out

  /** Logger levels (i.e., verbosity of output) */
  sealed abstract class Level(val level: Int, val name: String)
      extends Ordered[Level] {
    def compare(that: Level) = this.level - that.level
    override def toString = name
  }
  case object QUIET extends Level(0, "normal")
  case object NORMAL extends Level(1, "normal")
  case object DEBUG extends Level(2, "debug")
  case object VERBOSE extends Level(3, "verbose")

  /** Currend logger level */
  var level: Level = DEBUG

  def print(str: Any, lvl: Level = NORMAL): Unit = {
    if (lvl <= level) {
      val time = Calendar.getInstance.getTime
      output println s"[$lvl][$time] $str"
    }
  }

  def timed[A](expr: => A, desc: String = "", lvl: Level = NORMAL): A = {
    val t0 = System.currentTimeMillis()
    print(s"$desc (START)", lvl)
    val result = expr
    val t1 = System.currentTimeMillis()
    print(s"$desc (END): ${(t1 - t0).toFloat / 1000}s", lvl)
    result
  }

}
