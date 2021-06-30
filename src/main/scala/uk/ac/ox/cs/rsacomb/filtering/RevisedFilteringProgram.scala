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

package uk.ac.ox.cs.rsacomb.filtering

//import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Atom,
  FilterAtom,
  Rule,
  TupleTableAtom,
  TupleTableName,
  BodyFormula,
  Negation
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  IRI,
  FunctionCall,
  Term,
  Variable
}
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.suffix.{RSASuffix, Forward, Backward}
import uk.ac.ox.cs.rsacomb.util.{RSA, RDFoxUtil}

object RDFoxDSL {

  import scala.collection.JavaConverters._

  implicit class MyVariable(private val str: StringContext) extends AnyVal {
    def v(args: Any*): Variable = Variable.create(s"${str.s(args: _*)}i")
  }

}

/** Factory for [[uk.ac.ox.cs.rsacomb.FilteringProgram FilteringProgram]] */
object RevisedFilteringProgram {

  /** Create a new FilteringProgram instance.
    *
    * @param query CQ to be converted into logic rules.
    */
  def apply(query: ConjunctiveQuery): RevisedFilteringProgram =
    new RevisedFilteringProgram(query)

}

/** Filtering Program generator
  *
  * Handles the conversion of a CQ into a set of logic rules,
  * representing the filtering step of the RSA combined approach.
  *
  * Instances can be created using the companion object.
  */
class RevisedFilteringProgram(val query: ConjunctiveQuery)
    extends FilteringProgram {

  import RDFoxDSL._

  /** Extends capabilities of
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]]
    */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAtom._

  /** Implicit parameter used in RSA internal predicates.
    *
    * @see [[uk.ac.ox.cs.rsacomb.util.RSA]] for more information.
    */
  implicit private[this] val _query = query

  /** Helpers */

  //private def v(name: String): Term = Variable.create(s"${name}i")
  private def not(atom: TupleTableAtom): BodyFormula = Negation.create(atom)

  private def named(x: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, IRI.RDF_TYPE, RSA.NAMED)
  private def congruent(x: Term, y: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, RSA.CONGRUENT, y)
  private def skolem(skolem: Term, terms: List[Term]): TupleTableAtom =
    TupleTableAtom.create(TupleTableName.SKOLEM, (terms :+ skolem): _*)
  private def QM(x: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, IRI.RDF_TYPE, RSA("QM"))
  private def FK(x: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, IRI.RDF_TYPE, RSA("FK"))
  private def SP(x: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, IRI.RDF_TYPE, RSA("SP"))
  private def NI(x: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, IRI.RDF_TYPE, RSA("NI"))
  private def Ans(x: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, IRI.RDF_TYPE, RSA("Ans"))
  private def ID(x: Term, y: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, RSA("ID"), y)
  private def AQ(suffix: RSASuffix, x: Term, y: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, RSA("AQ"), y) << suffix
  private def TQ(suffix: RSASuffix, x: Term, y: Term): TupleTableAtom =
    TupleTableAtom.rdf(x, RSA("TQ"), y) << suffix

  /** Rule generating the instances of the predicate `rsa:NI`.
    *
    * According to the original paper, the set of `rsa:NI` is defined as
    * the set of constants that are equal (w.r.t. the congruence
    * relation represented by `rsa:Congruent`) to a constant in the
    * original ontology.
    *
    * @note that the set of `rsa:Named` constants is always a subset of
    * the set of `rsa:NI`s.
    *
    * @note in the paper, instances of `rsa:NI` are introduced as facts
    * during the canonical model computation. By definition of the
    * predicate, this is not feasible, and the instances are instead
    * generate in the filtering program using a logic rule.
    */
  val nis: Rule = Rule.create(NI(v"X"), named(v"Y"), congruent(v"X", v"Y"))

  /** Collection of filtering program rules. */
  val rules: List[Rule] =
    nis :: {

      val variables = query.answer ::: query.bounded

      /** Generates all possible, unfiltered answers.
        *
        * @note corresponds to rule 1 in Table 3 in the paper.
        */
      val r1 =
        Rule.create(QM(v"K"), (query.atoms :+ skolem(v"K", variables)): _*)

      /** Initializes instances of `rsa:ID`.
        *
        * They are initialized as a minimal congruence relation over the
        * positions of the existential variables in the query which are
        * mapped to anonymous terms.
        *
        * @note corresponds to rules 3x in Table 3.
        */
      val r3a =
        for ((v, i) <- query.bounded.zipWithIndex)
          yield Rule.create(
            ID(v"K", v"S"),
            QM(v"K"),
            skolem(v"K", variables),
            not(NI(v)),
            skolem(v"S", variables :+ RSA(i) :+ RSA(i))
          )
      val r3b = Rule.create(
        ID(v"K", v"T"),
        ID(v"K", v"S"),
        skolem(v"S", variables :+ v"U" :+ v"V"),
        skolem(v"T", variables :+ v"V" :+ v"U")
      )
      val r3c = Rule.create(
        ID(v"K1", v"Q"),
        QM(v"K1"),
        ID(v"K2", v"S"),
        FilterAtom.create(FunctionCall.equal(v"K1", v"K2")),
        skolem(v"S", variables :+ v"U" :+ v"V"),
        ID(v"K3", v"T"),
        FilterAtom.create(FunctionCall.equal(v"K1", v"K3")),
        skolem(v"T", variables :+ v"V" :+ v"W"),
        skolem(v"Q", variables :+ v"U" :+ v"W")
      )

      /** Detects forks in the canonical model.
        *
        * @note corresponds to rules 4x in Table 3.
        */
      val r4a = for {
        role1 <- query.atoms filter (_.isRoleAssertion)
        index1 = query.bounded indexOf (role1.getArguments get 2)
        if index1 >= 0
        role2 <- query.atoms filter (_.isRoleAssertion)
        index2 = query.bounded indexOf (role2.getArguments get 2)
        if index2 >= 0
      } yield Rule.create(
        FK(v"K"),
        ID(v"K", v"S"),
        skolem(v"S", variables :+ RSA(index1) :+ RSA(index2)),
        role1 << Forward,
        role2 << Forward,
        not(RSA.Congruent(role1.getArguments get 0, role2.getArguments get 0))
      )
      val r4b = for {
        role1 <- query.atoms filter (_.isRoleAssertion)
        index1 = query.bounded indexOf (role1.getArguments get 2)
        if index1 >= 0
        role2 <- query.atoms filter (_.isRoleAssertion)
        index2 = query.bounded indexOf (role2.getArguments get 0)
        if index2 >= 0
      } yield Rule.create(
        FK(v"K"),
        ID(v"K", v"S"),
        skolem(v"S", variables :+ RSA(index1) :+ RSA(index2)),
        role1 << Forward,
        role2 << Backward,
        not(RSA.Congruent(role1.getArguments get 0, role2.getArguments get 2))
      )
      val r4c = for {
        role1 <- query.atoms filter (_.isRoleAssertion)
        index1 = query.bounded indexOf (role1.getArguments get 0)
        if index1 >= 0
        role2 <- query.atoms filter (_.isRoleAssertion)
        index2 = query.bounded indexOf (role2.getArguments get 0)
        if index2 >= 0
      } yield Rule.create(
        FK(v"K"),
        ID(v"K", v"S"),
        skolem(v"S", variables :+ RSA(index1) :+ RSA(index2)),
        role1 << Backward,
        role2 << Backward,
        not(RSA.Congruent(role1.getArguments get 2, role2.getArguments get 2))
      )

      /** Recursively propagates `rsa:ID` predicate.
        *
        * @note corresponds to rules 5x in Table 3.
        */
      val r5a = for {
        role1 <- query.atoms filter (_.isRoleAssertion)
        r1arg0 = role1.getArguments get 0
        if query.bounded contains r1arg0
        r1arg2 = role1.getArguments get 2
        if query.bounded contains r1arg2
        role2 <- query.atoms filter (_.isRoleAssertion)
        r2arg0 = role2.getArguments get 0
        if query.bounded contains r2arg0
        r2arg2 = role2.getArguments get 2
        if query.bounded contains r2arg2
      } yield Rule.create(
        ID(v"K", v"T"),
        ID(v"K", v"S"),
        skolem(
          v"S",
          variables :+
            RSA(query.bounded indexOf r1arg2) :+
            RSA(query.bounded indexOf r2arg2)
        ),
        RSA.Congruent(r1arg0, r2arg0),
        role1 << Forward,
        role2 << Forward,
        not(NI(r1arg0)),
        skolem(
          v"T",
          variables :+
            RSA(query.bounded indexOf r1arg0) :+
            RSA(query.bounded indexOf r2arg0)
        )
      )
      val r5b = for {
        role1 <- query.atoms filter (_.isRoleAssertion)
        r1arg0 = role1.getArguments get 0
        if query.bounded contains r1arg0
        r1arg2 = role1.getArguments get 2
        if query.bounded contains r1arg2
        role2 <- query.atoms filter (_.isRoleAssertion)
        r2arg0 = role2.getArguments get 0
        if query.bounded contains r2arg0
        r2arg2 = role2.getArguments get 2
        if query.bounded contains r2arg2
      } yield Rule.create(
        ID(v"K", v"T"),
        ID(v"K", v"S"),
        skolem(
          v"S",
          variables :+
            RSA(query.bounded indexOf r1arg2) :+
            RSA(query.bounded indexOf r2arg0)
        ),
        RSA.Congruent(r1arg0, r2arg2),
        role1 << Forward,
        role2 << Backward,
        not(RSA.NI(r1arg0)),
        skolem(
          v"T",
          variables :+
            RSA(query.bounded indexOf r1arg0) :+
            RSA(query.bounded indexOf r2arg2)
        )
      )
      val r5c = for {
        role1 <- query.atoms filter (_.isRoleAssertion)
        r1arg0 = role1.getArguments get 0
        if query.bounded contains r1arg0
        r1arg2 = role1.getArguments get 2
        if query.bounded contains r1arg2
        role2 <- query.atoms filter (_.isRoleAssertion)
        r2arg0 = role2.getArguments get 0
        if query.bounded contains r2arg0
        r2arg2 = role2.getArguments get 2
        if query.bounded contains r2arg2
      } yield Rule.create(
        ID(v"K", v"T"),
        ID(v"K", v"S"),
        skolem(
          v"S",
          variables :+
            RSA(query.bounded indexOf r1arg0) :+
            RSA(query.bounded indexOf r2arg0)
        ),
        RSA.Congruent(r1arg2, r2arg2),
        role1 << Backward,
        role2 << Backward,
        not(RSA.NI(r1arg2)),
        skolem(
          v"T",
          variables :+
            RSA(query.bounded indexOf r1arg2) :+
            RSA(query.bounded indexOf r2arg2)
        )
      )

      /** Detect cycles in the canonical model.
        *
        * Cycles are detected by introducing a new predicate `rsa:AQ`
        * and computing its transitive closure. Cycles are computed from
        * forward and backward roles separately.
        *
        * @note corresponds to rules 6,7x in Table 3.
        */
      val r6 = for {
        role <- query.atoms filter (_.isRoleAssertion)
        index0 = query.bounded indexOf (role.getArguments get 0)
        if index0 >= 0
        index2 = query.bounded indexOf (role.getArguments get 2)
        if index2 >= 0
        suffix <- Seq(Forward, Backward)
      } yield Rule.create(
        AQ(suffix, v"K1", v"Q"),
        ID(v"K1", v"S"),
        skolem(v"S", variables :+ RSA(index0) :+ v"V"),
        ID(v"K2", v"T"),
        FilterAtom.create(FunctionCall.equal(v"K1", v"K2")),
        skolem(v"T", variables :+ RSA(index2) :+ v"W"),
        role << suffix,
        skolem(v"Q", variables :+ v"V" :+ v"W")
      )
      val r7a =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            TQ(suffix, v"K", v"S"),
            AQ(suffix, v"K", v"S")
          )
      val r7b =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            TQ(suffix, v"K1", v"Q"),
            AQ(suffix, v"K1", v"S"),
            skolem(v"S", variables :+ v"U" :+ v"V"),
            TQ(suffix, v"K2", v"T"),
            FilterAtom.create(FunctionCall.equal(v"K1", v"K2")),
            skolem(v"T", variables :+ v"V" :+ v"W"),
            skolem(v"Q", variables :+ v"U" :+ v"W")
          )

      /** Flag spurious answers.
        *
        * @note corresponds to rules 8x in Table 3.
        */
      val r8a =
        for (v <- query.answer)
          yield Rule.create(
            SP(v"K"),
            QM(v"K"),
            skolem(v"K", variables),
            not(RSA.Named(v))
          )
      val r8b = Rule.create(
        SP(v"K"),
        FK(v"K")
      )
      val r8c =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            SP(v"K"),
            TQ(suffix, v"K", v"S"),
            skolem(v"S", variables :+ v"V" :+ v"V")
          )

      /** Determine answers to the query
        *
        * Answers are identified by predicate `rsa:Ans`. In case the
        * input query is a BCQ (answer is just true/false), we derive
        * `rsa:Ans` for a fresh constant `c`. Later on we can query for
        * instances of `rsa:Ans` as follows
        *
        * {{{
        *   ASK { ?X a rsa:Ans }
        * }}}
        *
        * to determine whether the query is true or false.
        *
        * @note corresponds to rule 9 in Table 3.
        */
      val r9 = Rule.create(
        Ans(v"K"),
        QM(v"K"),
        not(SP(v"K"))
      )

      (r1 :: r3a ::: r3b :: r3c :: r4a ::: r4b ::: r4c ::: r5a ::: r5b ::: r5c ::: r6 ::: r7b ::: r7a ::: r8a ::: r8b :: r8c ::: r9 :: List())
    }

  val answerQuery: String = {
    val arity = query.answer.size
    if (arity > 0) {
      val answer = query.answer mkString " "
      val bounded = query.bounded mkString " "
      s"""
        SELECT $answer
        WHERE {
            ?K a rsa:Ans .
            TT <http://oxfordsemantic.tech/RDFox#SKOLEM> { $answer $bounded ?K } .
        }
      """
    } else {
      "ASK { ?X a rsa:Ans }"
    }
  }

}
