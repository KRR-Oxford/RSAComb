package uk.ac.ox.cs.rsacomb.sparql

import java.util.{Map => JMap, HashMap => JHashMap}
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.DataStoreConnection
import tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom
import tech.oxfordsemantic.jrdfox.logic.expression.Variable
import tech.oxfordsemantic.jrdfox.logic.sparql.pattern.{
  ConjunctionPattern,
  QueryPattern,
  TriplePattern
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import uk.ac.ox.cs.rsacomb.util.RDFoxUtil

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
  def parse(
      query: String,
      prefixes: Prefixes = new Prefixes()
  ): Option[ConjunctiveQuery] =
    RDFoxUtil.parseSelectQuery(query, prefixes).map(ConjunctiveQuery(_))

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

  /** Simplify conversion between Java and Scala collections */
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
  val bcq: Boolean = select.isEmpty && !query.getAllPossibleVariables

  /** Returns the query body as a sequence of atoms (triples). */
  val atoms: List[TupleTableAtom] =
    where match {
      case b: ConjunctionPattern => {
        b.getConjuncts.toList.flatMap { conj: QueryPattern =>
          conj match {
            case c: TriplePattern =>
              Seq(
                TupleTableAtom.rdf(c.getSubject, c.getPredicate, c.getObject)
              )
            case _ => List()
          }
        }
      }
      case _ => List()
    }

  /** Returns the full collection of variables involved in the query. */
  val variables: List[Variable] = (where match {
    case b: ConjunctionPattern => {
      b.getConjuncts.toList.flatMap { conj: QueryPattern =>
        conj match {
          case c: TriplePattern =>
            Set(c.getSubject, c.getPredicate, c.getObject).collect {
              case v: Variable => v
            }
          case _ => List()
        }
      }
    }
    case _ => List()
  }).distinct

  /** Returns the collection of answer variables in the query. */
  val answer: List[Variable] =
    if (query.getAllPossibleVariables)
      variables
    else
      select.map(_.getVariable).toList.distinct

  /** Returns the collection of bounded (existential) variables in the query. */
  val bounded: List[Variable] = variables diff answer

  override def toString(): String = query.toString
}
