package uk.ac.ox.cs.rsacomb.filtering

import tech.oxfordsemantic.jrdfox.logic.datalog.Rule
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.util.Versioned

sealed trait FilterType
object FilterType {
  case object NAIVE extends FilterType
  case object REVISED extends FilterType
}

object FilteringProgram extends Versioned[FilterType] {

  import FilterType._

  type Result = (ConjunctiveQuery) => FilteringProgram

  def apply(t: FilterType): (ConjunctiveQuery) => FilteringProgram =
    t match {
      case NAIVE   => NaiveFilteringProgram(_)
      case REVISED => RevisedFilteringProgram(_)
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

  /** Query to be used to retrieve the answers */
  def answerQuery: String

  /** Pretty-print filtering rule */
  override def toString(): String = rules mkString "\n"
}
