package uk.ac.ox.cs.rsacomb.filtering

import tech.oxfordsemantic.jrdfox.logic.datalog.Rule
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.util.Versioned

sealed trait FilterType
object FilterType {
  case object FILTER_NAIVE extends FilterType
  case object FILTER_REVISED_V1 extends FilterType
}

object FilteringProgram extends Versioned[FilterType] {

  import FilterType._

  type Result = (ConjunctiveQuery) => FilteringProgram

  def apply(t: FilterType): (ConjunctiveQuery) => FilteringProgram =
    t match {
      case FILTER_NAIVE      => NaiveFilteringProgram(_)
      case FILTER_REVISED_V1 => NaiveFilteringProgram(_)
    }
}

/** Filtering Program generator
  *
  * Handles the conversion of a CQ into a set of logic rules,
  * representing the filtering step of the RSA combined approach.
  */
trait FilteringProgram {

  /** Query from which the filtering program is generated */
  val query: ConjunctiveQuery

  /** Collection of filtering program rules. */
  def rules: List[Rule]

  /** Pretty-print filtering rule */
  override def toString(): String = rules mkString "\n"
}
