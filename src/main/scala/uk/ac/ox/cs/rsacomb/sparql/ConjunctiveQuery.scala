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

package uk.ac.ox.cs.rsacomb.sparql

import java.util.{Map => JMap, HashMap => JHashMap}
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.DataStoreConnection
import tech.oxfordsemantic.jrdfox.logic.datalog.{TupleTableAtom, TupleTableName}
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
  def apply(id: Int, query: SelectQuery): ConjunctiveQuery =
    new ConjunctiveQuery(id, query)

  /** Creates a new ConjunctiveQuery from a query string
    *
    * @param query a string representing the query in SPARQL format
    * @param prefixes additional prefixes used in the query. Defaults to
    *                 an empty set of prefixes.
    * @return an [[scala.Option]] containing a ConjunctiveQuery if the
    *         input query represents one, None is returned otherwise.
    */
  def parse(
      id: Int,
      query: String,
      prefixes: Prefixes = new Prefixes()
  ): Option[ConjunctiveQuery] =
    RDFoxUtil.parseSelectQuery(query, prefixes).map(ConjunctiveQuery(id, _))

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
    val id: Int,
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
  def atoms(graph: TupleTableName): List[TupleTableAtom] =
    where
      .asInstanceOf[ConjunctionPattern]
      .getConjuncts
      .collect { case t: TriplePattern =>
        TupleTableAtom.create(graph, t.getSubject, t.getPredicate, t.getObject)
      }
  // where match {
  //   case b: ConjunctionPattern => {
  //     b.getConjuncts.toList.flatMap { conj: QueryPattern =>
  //       conj match {
  //         case c: TriplePattern =>
  //           Seq(
  //             TupleTableAtom.rdf(c.getSubject, c.getPredicate, c.getObject)
  //           )
  //         case _ => List()
  //       }
  //     }
  //   }
  //   case _ => List()
  // }

  /** Returns the full collection of variables involved in the query. */
  val variables: List[Variable] =
    where
      .asInstanceOf[ConjunctionPattern]
      .getConjuncts
      .collect { case t: TriplePattern =>
        Set(t.getSubject, t.getPredicate, t.getObject).collect {
          case v: Variable => v
        }
      }
      .flatten
      .distinct
  // (where match {
  //   case b: ConjunctionPattern => {
  //     b.getConjuncts.toList.flatMap { conj: QueryPattern =>
  //       conj match {
  //         case c: TriplePattern =>
  //           Set(c.getSubject, c.getPredicate, c.getObject).collect {
  //             case v: Variable => v
  //           }
  //         case _ => List()
  //       }
  //     }
  //   }
  //   case _ => List()
  // }).distinct

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
