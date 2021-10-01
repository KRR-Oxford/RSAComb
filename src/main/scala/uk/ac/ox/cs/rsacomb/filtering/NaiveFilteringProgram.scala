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
  Rule,
  TupleTableAtom,
  TupleTableName,
  BodyFormula,
  Negation
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  IRI,
  Literal,
  Term,
  Variable
}
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.suffix.{Forward, Backward, Nth}
import uk.ac.ox.cs.rsacomb.util.{DataFactory, RSA, RDFoxUtil}

/** Factory for [[uk.ac.ox.cs.rsacomb.FilteringProgram FilteringProgram]] */
object NaiveFilteringProgram {

  /** Create a new FilteringProgram instance.
    *
    * @param source source named graph for the filtering program.
    * @param target target named graph for the filtering program.
    * @param query CQ to be converted into logic rules.
    */
  def apply(
      source: IRI,
      target: IRI,
      query: ConjunctiveQuery
  ): FilteringProgram =
    new NaiveFilteringProgram(source, target, query)
}

/** Filtering Program generator
  *
  * Handles the conversion of a CQ into a set of logic rules,
  * representing the filtering step of the RSA combined approach.
  *
  * Instances can be created using the companion object.
  */
class NaiveFilteringProgram(
    val source: IRI,
    val target: IRI,
    val query: ConjunctiveQuery
) extends FilteringProgram {

  /** Extends capabilities of
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]]
    */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAtom._

  /** Simplify conversion to RDFox specific types */
  import uk.ac.ox.cs.rsacomb.implicits.RDFox._

  /** Simplify conversion between Java and Scala `List`s */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Implicit parameter used in RSA internal predicates.
    *
    * @see [[uk.ac.ox.cs.rsacomb.util.RSA]] for more information.
    */
  implicit private[this] val _query = query

  /** General purpose variables used in rule instantiation.
    *
    * This is done to avoid creating new variables every time.
    */
  private val varX = Variable.create("X")
  private val varY = Variable.create("Y")
  private val varZ = Variable.create("Z")
  private val varV = Variable.create("V")
  private val varU = Variable.create("U")
  private val varW = Variable.create("W")

  /** `TupleTableName`s for the source/targer named graphs */
  val tts: TupleTableName = TupleTableName.create(source.getIRI)
  implicit val ttt: TupleTableName = TupleTableName.create(target.getIRI)

  /** Set of atoms in the body of the query */
  val queryBody: List[TupleTableAtom] = query.atoms(tts)

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
  val nis: Rule =
    Rule.create(
      RSA.NI(varX),
      RSA.Congruent(varX, varY)(tts),
      RSA.Named(varY)(tts)
    )

  /** Collection of filtering program rules. */
  val rules: List[Rule] =
    nis :: {

      /** Negates a [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]] */
      def not(atom: TupleTableAtom): BodyFormula = Negation.create(atom)

      /** Generates all possible, unfiltered answers.
        *
        * @note corresponds to rule 1 in Table 3 in the paper.
        */
      val r1 = Rule.create(RSA.QM, queryBody: _*)

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
          yield Rule.create(RSA.ID(RSA(i), RSA(i)), RSA.QM, not(RSA.NI(v)))
      val r3b = Rule.create(RSA.ID(varV, varU), RSA.ID(varU, varV))
      val r3c =
        Rule.create(RSA.ID(varU, varW), RSA.ID(varU, varV), RSA.ID(varV, varW))

      /** Detects forks in the canonical model.
        *
        * @note corresponds to rules 4x in Table 3.
        */
      val r4a = for {
        role1 <- queryBody filter (_.isRoleAssertion)
        index1 = query.bounded indexOf (role1.getArguments get 2)
        if index1 >= 0
        role2 <- queryBody filter (_.isRoleAssertion)
        index2 = query.bounded indexOf (role2.getArguments get 2)
        if index2 >= 0
      } yield Rule.create(
        RSA.FK,
        RSA.ID(RSA(index1), RSA(index2)),
        role1 << Forward,
        role2 << Forward,
        not(
          TupleTableAtom.create(
            tts,
            role1.getArguments get 0,
            RSA.CONGRUENT,
            role2.getArguments get 0
          )
        )
      )
      val r4b = for {
        role1 <- queryBody filter (_.isRoleAssertion)
        index1 = query.bounded indexOf (role1.getArguments get 2)
        if index1 >= 0
        role2 <- queryBody filter (_.isRoleAssertion)
        index2 = query.bounded indexOf (role2.getArguments get 0)
        if index2 >= 0
      } yield Rule.create(
        RSA.FK,
        RSA.ID(RSA(index1), RSA(index2)),
        role1 << Forward,
        role2 << Backward,
        not(
          TupleTableAtom.create(
            tts,
            role1.getArguments get 0,
            RSA.CONGRUENT,
            role2.getArguments get 2
          )
        )
      )
      val r4c = for {
        role1 <- queryBody filter (_.isRoleAssertion)
        index1 = query.bounded indexOf (role1.getArguments get 0)
        if index1 >= 0
        role2 <- queryBody filter (_.isRoleAssertion)
        index2 = query.bounded indexOf (role2.getArguments get 0)
        if index2 >= 0
      } yield Rule.create(
        RSA.FK,
        RSA.ID(RSA(index1), RSA(index2)),
        role1 << Backward,
        role2 << Backward,
        not(
          TupleTableAtom.create(
            tts,
            role1.getArguments get 2,
            RSA.CONGRUENT,
            role2.getArguments get 2
          )
        )
      )

      /** Recursively propagates `rsa:ID` predicate.
        *
        * @note corresponds to rules 5x in Table 3.
        */
      val r5a = for {
        role1 <- queryBody filter (_.isRoleAssertion)
        r1arg0 = role1.getArguments get 0
        if query.bounded contains r1arg0
        r1arg2 = role1.getArguments get 2
        if query.bounded contains r1arg2
        role2 <- queryBody filter (_.isRoleAssertion)
        r2arg0 = role2.getArguments get 0
        if query.bounded contains r2arg0
        r2arg2 = role2.getArguments get 2
        if query.bounded contains r2arg2
      } yield Rule.create(
        RSA.ID(
          RSA(query.bounded indexOf r1arg0),
          RSA(query.bounded indexOf r2arg0)
        ),
        RSA.ID(
          RSA(query.bounded indexOf r1arg2),
          RSA(query.bounded indexOf r2arg2)
        ),
        TupleTableAtom.create(tts, r1arg0, RSA.CONGRUENT, r2arg0),
        role1 << Forward,
        role2 << Forward,
        not(RSA.NI(r1arg0))
      )
      val r5b = for {
        role1 <- queryBody filter (_.isRoleAssertion)
        r1arg0 = role1.getArguments get 0
        if query.bounded contains r1arg0
        r1arg2 = role1.getArguments get 2
        if query.bounded contains r1arg2
        role2 <- queryBody filter (_.isRoleAssertion)
        r2arg0 = role2.getArguments get 0
        if query.bounded contains r2arg0
        r2arg2 = role2.getArguments get 2
        if query.bounded contains r2arg2
      } yield Rule.create(
        RSA.ID(
          RSA(query.bounded indexOf r1arg0),
          RSA(query.bounded indexOf r2arg2)
        ),
        RSA.ID(
          RSA(query.bounded indexOf r1arg2),
          RSA(query.bounded indexOf r2arg0)
        ),
        TupleTableAtom.create(tts, r1arg0, RSA.CONGRUENT, r2arg2),
        role1 << Forward,
        role2 << Backward,
        not(RSA.NI(r1arg0))
      )
      val r5c = for {
        role1 <- queryBody filter (_.isRoleAssertion)
        r1arg0 = role1.getArguments get 0
        if query.bounded contains r1arg0
        r1arg2 = role1.getArguments get 2
        if query.bounded contains r1arg2
        role2 <- queryBody filter (_.isRoleAssertion)
        r2arg0 = role2.getArguments get 0
        if query.bounded contains r2arg0
        r2arg2 = role2.getArguments get 2
        if query.bounded contains r2arg2
      } yield Rule.create(
        RSA.ID(
          RSA(query.bounded indexOf r1arg2),
          RSA(query.bounded indexOf r2arg2)
        ),
        RSA.ID(
          RSA(query.bounded indexOf r1arg0),
          RSA(query.bounded indexOf r2arg0)
        ),
        TupleTableAtom.create(tts, r1arg2, RSA.CONGRUENT, r2arg2),
        role1 << Backward,
        role2 << Backward,
        not(RSA.NI(r1arg2))
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
        role <- queryBody filter (_.isRoleAssertion)
        index0 = query.bounded indexOf (role.getArguments get 0)
        if index0 >= 0
        index2 = query.bounded indexOf (role.getArguments get 2)
        if index2 >= 0
        suffix <- Seq(Forward, Backward)
      } yield Rule.create(
        RSA.AQ(suffix, varV, varW),
        role << suffix,
        RSA.ID(RSA(index0), varV),
        RSA.ID(RSA(index2), varW)
      )
      val r7a =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            RSA.TQ(suffix, varU, varV),
            RSA.AQ(suffix, varU, varV)
          )
      val r7b =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            RSA.TQ(suffix, varU, varW),
            RSA.AQ(suffix, varU, varV),
            RSA.TQ(suffix, varV, varW)
          )

      /** Flag spurious answers.
        *
        * @note corresponds to rules 8x in Table 3.
        */
      val r8a =
        for (v <- query.answer)
          yield Rule.create(
            RSA.SP,
            RSA.QM,
            not(
              TupleTableAtom.create(tts, v, IRI.RDF_TYPE, RSA.NAMED)
            )
          )
      val r8b = Rule.create(RSA.SP, RSA.FK)
      val r8c =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            RSA.SP,
            RSA.TQ(suffix, varV, varV)
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
      val r9 = Rule.create(RSA.Ans, RSA.QM, not(RSA.SP))

      (r1 ::
        r3a ::: r3b :: r3c ::
        r4a ::: r4b ::: r4c :::
        r5a ::: r5b ::: r5c :::
        r6 ::: r7b ::: r7a :::
        r8a ::: r8b :: r8c :::
        r9 :: List()) map reify
    }

  /** Reify a [[tech.oxfordsemantic.jrdfox.logic.datalog.Rule Rule]].
    *
    * This is needed because RDFox supports only predicates of arity 1
    * or 2, but the filtering program uses predicates with higher arity.
    *
    * @note we can perform a reification of the atoms thanks to the
    * built-in `SKOLEM` funtion of RDFox.
    */
  def reify(rule: Rule): Rule = {
    val (sk, as) = rule.getHead.map(reify).unzip
    val head: List[TupleTableAtom] = as.flatten
    val skolem: List[BodyFormula] = sk.flatten
    val body: List[BodyFormula] = rule.getBody.map(reify).flatten
    Rule.create(head, skolem ::: body)
  }

  /** Reify a [[tech.oxfordsemantic.jrdfox.logic.datalog.BodyFormula BodyFormula]]. */
  private def reify(formula: BodyFormula): List[BodyFormula] = {
    formula match {
      case atom: TupleTableAtom => reify(atom)._2
      case neg: Negation => {
        val (sk, as) = neg.getNegatedAtoms
          .map({
            case a: TupleTableAtom => reify(a)
            case a                 => (None, List(a))
          })
          .unzip
        val skolem =
          sk.flatten.map(_.getArguments.last).collect { case v: Variable => v }
        val atoms = as.flatten
        List(Negation.create(skolem, atoms))
      }
      case other => List(other)
    }
  }

  /** Reify a [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]]. */
  private def reify(atom: TupleTableAtom)(implicit
      fresh: DataFactory
  ): (Option[TupleTableAtom], List[TupleTableAtom]) = {
    if (atom.getArguments.length == 3) {
      (None, List(atom))
    } else {
      val varS: Variable = fresh.getVariable
      val (pred :: args): List[Term] = atom.getArguments
      val name = pred.asInstanceOf[IRI].getIRI
      val skolem = TupleTableAtom.create(
        TupleTableName.SKOLEM,
        Literal.create(name, Datatype.XSD_STRING) +: args :+ varS
      )
      val triple =
        TupleTableAtom.create(atom.getTupleTableName, varS, IRI.RDF_TYPE, pred)
      val triples = args.zipWithIndex
        .map { case (a, i) =>
          TupleTableAtom.create(atom.getTupleTableName, varS, name :: Nth(i), a)
        }
      (Some(skolem), triple :: triples)
    }
  }

  val answerQuery =
    RDFoxUtil.buildDescriptionQuery(target, RSA.ANS, query.answer.size)

}
