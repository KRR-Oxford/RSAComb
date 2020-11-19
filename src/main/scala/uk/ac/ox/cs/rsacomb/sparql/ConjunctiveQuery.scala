package uk.ac.ox.cs.rsacomb.sparql

import java.util.{Map => JMap, HashMap => JHashMap}
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.DataStoreConnection
import tech.oxfordsemantic.jrdfox.logic.expression.Variable
import tech.oxfordsemantic.jrdfox.logic.sparql.pattern.{
  ConjunctionPattern,
  QueryPattern,
  TriplePattern
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import uk.ac.ox.cs.rsacomb.util.RDFoxHelpers

/** Factory for [[uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery]]. */
object ConjunctiveQuery {

  /** Creates a new ConjunctiveQuery instance.
    *
    * @param query `SelectQuery` instance representing the actual query
    */
  def apply(query: SelectQuery): ConjunctiveQuery =
    new ConjunctiveQuery(query)

  /** Creates a new ConjunctiveQuery from a query string
    *
    * @param query a string representing the query in SPARQL format
    * @param prefixes additional prefixes used in the query. Defaults to
    *                 an empty set of prefixes.
    * @return an [[scala.Option]] containing a ConjunctiveQuery if the
    *         input query represents one, None is returned otherwise.
    */
  def apply(
      query: String,
      prefixes: Prefixes = new Prefixes()
  ): Option[ConjunctiveQuery] =
    RDFoxHelpers.parseSelectQuery(query, prefixes).map(ConjunctiveQuery(_))

}

/** A conjunctive query
  *
  * A thin layer around
  * [[tech.oxfordsemantics.jrdfox.logic.sparql.statement.SelectQuery]].
  *
  * Instances should be created using the companion object.
  *
  * @todo additional checks need to be performed in order for a
  * `SelectQuery` to be considered a conjunctive query.
  */
class ConjunctiveQuery(
    query: SelectQuery,
    val prefixes: Prefixes = new Prefixes()
) {

  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** SELECT section of the SPARQL query.
    *
    * Simply exposes the underlying `getSelection` method in
    * [[tech.oxfordsemantics.jrdfox.logic.sparql.statement.SelectQuery]].
    */
  val select = query.getSelection

  /** WHERE section of the SPARQL query.
    *
    * Simply exposes the underlying `getWherePattern` method in
    * [[tech.oxfordsemantics.jrdfox.logic.sparql.statement.QueryBody]].
    */
  val where = query.getQueryBody.getWherePattern

  /** Returns true if it is a boolean CQ.
    *
    * @note checking for `select` being empty is not enough. When a
    * query selects '''all''' variables with `*`, `select` is empty as
    * well.
    */
  val boolean: Boolean = select.isEmpty && !query.getAllPossibleVariables

  /** Returns the full set of variables involved in the query. */
  val variables: Set[Variable] =
    where match {
      case b: ConjunctionPattern => {
        b.getConjuncts.toSet.flatMap { conj: QueryPattern =>
          conj match {
            case c: TriplePattern =>
              Set(c.getSubject, c.getPredicate, c.getObject).collect {
                case v: Variable => v
              }
            case _ => Set()
          }
        }
      }
      case _ => Set()
    }

  /** Returns the collection of answer variables in the query. */
  val answer: Set[Variable] =
    if (query.getAllPossibleVariables)
      variables
    else
      select.map(_.getVariable).toSet

  /** Returns the collection of bounded (existential) variables in the query. */
  val bounded: Set[Variable] = variables &~ answer

  /** Returns the answers to a query
    *
    * @param data data store against which the query is executed
    * @param opts additional options passed to RDFox
    * @return a new [[ConjunctiveQueryAnswers]] instance containing the
    *         collection of answers.
    */
  def answers(
      data: DataStoreConnection,
      opts: JMap[String, String] = new JHashMap[String, String]()
  ): ConjunctiveQueryAnswers =
    new ConjunctiveQueryAnswers(
      boolean,
      RDFoxHelpers.submitSelectQuery(data, query, opts)
    )

  override def toString(): String = query.toString
}
