package uk.ac.ox.cs.rsacomb.filtering

//import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.datalog.{
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

  private def ?(name: String): Term = Variable.create(s"${name}i")
  private def not(atom: TupleTableAtom): BodyFormula = Negation.create(atom)
  private def skolem(terms: List[Term]): TupleTableAtom =
    TupleTableAtom.create(TupleTableName.SKOLEM, terms: _*)

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
  val nis: Rule =
    Rule.create(NI(?("X")), RSA.Congruent(?("X"), ?("Y")), RSA.Named(?("Y")))

  /** Collection of filtering program rules. */
  val rules: List[Rule] =
    nis :: {

      val variables = query.answer ::: query.bounded

      /** Generates all possible, unfiltered answers.
        *
        * @note corresponds to rule 1 in Table 3 in the paper.
        */
      val r1 =
        Rule.create(
          QM(?("K")),
          (query.atoms :+ skolem(variables :+ ?("K"))): _*
        )

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
            ID(?("K"), ?("S")),
            QM(?("K")),
            skolem(variables :+ ?("K")),
            not(NI(v)),
            skolem(variables :+ RSA(i) :+ RSA(i) :+ ?("S"))
          )
      val r3b = Rule.create(
        ID(?("K"), ?("T")),
        ID(?("K"), ?("S")),
        skolem(variables :+ ?("U") :+ ?("V") :+ ?("S")),
        skolem(variables :+ ?("V") :+ ?("U") :+ ?("T"))
      )
      val r3c = Rule.create(
        ID(?("K1"), ?("Q")),
        QM(?("K1")),
        ID(?("K2"), ?("S")),
        FilterAtom.create(FunctionCall.equal(?("K1"), ?("K2"))),
        skolem(variables :+ ?("U") :+ ?("V") :+ ?("S")),
        ID(?("K3"), ?("T")),
        FilterAtom.create(FunctionCall.equal(?("K1"), ?("K3"))),
        skolem(variables :+ ?("V") :+ ?("W") :+ ?("T")),
        skolem(variables :+ ?("U") :+ ?("W") :+ ?("Q"))
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
        FK(?("K")),
        ID(?("K"), ?("S")),
        skolem(variables :+ RSA(index1) :+ RSA(index2) :+ ?("S")),
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
        FK(?("K")),
        ID(?("K"), ?("S")),
        skolem(variables :+ RSA(index1) :+ RSA(index2) :+ ?("S")),
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
        FK(?("K")),
        ID(?("K"), ?("S")),
        skolem(variables :+ RSA(index1) :+ RSA(index2) :+ ?("S")),
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
        ID(?("K"), ?("T")),
        ID(?("K"), ?("S")),
        skolem(
          variables :+
            RSA(query.bounded indexOf r1arg2) :+
            RSA(query.bounded indexOf r2arg2) :+
            ?("S")
        ),
        RSA.Congruent(r1arg0, r2arg0),
        role1 << Forward,
        role2 << Forward,
        not(NI(r1arg0)),
        skolem(
          variables :+
            RSA(query.bounded indexOf r1arg0) :+
            RSA(query.bounded indexOf r2arg0) :+
            ?("T")
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
        ID(?("K"), ?("T")),
        ID(?("K"), ?("S")),
        skolem(
          variables :+
            RSA(query.bounded indexOf r1arg2) :+
            RSA(query.bounded indexOf r2arg0) :+
            ?("S")
        ),
        RSA.Congruent(r1arg0, r2arg2),
        role1 << Forward,
        role2 << Backward,
        not(RSA.NI(r1arg0)),
        skolem(
          variables :+
            RSA(query.bounded indexOf r1arg0) :+
            RSA(query.bounded indexOf r2arg2) :+
            ?("T")
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
        ID(?("K"), ?("T")),
        ID(?("K"), ?("S")),
        skolem(
          variables :+
            RSA(query.bounded indexOf r1arg0) :+
            RSA(query.bounded indexOf r2arg0) :+
            ?("S")
        ),
        RSA.Congruent(r1arg2, r2arg2),
        role1 << Backward,
        role2 << Backward,
        not(RSA.NI(r1arg2)),
        skolem(
          variables :+
            RSA(query.bounded indexOf r1arg2) :+
            RSA(query.bounded indexOf r2arg2) :+
            ?("T")
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
        AQ(suffix, ?("K1"), ?("Q")),
        ID(?("K1"), ?("S")),
        skolem(variables :+ RSA(index0) :+ ?("V") :+ ?("S")),
        ID(?("K2"), ?("T")),
        FilterAtom.create(FunctionCall.equal(?("K1"), ?("K2"))),
        skolem(variables :+ RSA(index2) :+ ?("W") :+ ?("T")),
        role << suffix,
        skolem(variables :+ ?("V") :+ ?("W") :+ ?("Q"))
      )
      val r7a =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            TQ(suffix, ?("K"), ?("S")),
            AQ(suffix, ?("K"), ?("S"))
          )
      val r7b =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            TQ(suffix, ?("K1"), ?("Q")),
            AQ(suffix, ?("K1"), ?("S")),
            skolem(variables :+ ?("U") :+ ?("V") :+ ?("S")),
            TQ(suffix, ?("K2"), ?("T")),
            FilterAtom.create(FunctionCall.equal(?("K1"), ?("K2"))),
            skolem(variables :+ ?("V") :+ ?("W") :+ ?("T")),
            skolem(variables :+ ?("U") :+ ?("W") :+ ?("Q"))
          )

      /** Flag spurious answers.
        *
        * @note corresponds to rules 8x in Table 3.
        */
      val r8a =
        for (v <- query.answer)
          yield Rule.create(
            SP(?("K")),
            QM(?("K")),
            skolem(variables :+ ?("K")),
            not(RSA.Named(v))
          )
      val r8b = Rule.create(
        SP(?("K")),
        FK(?("K"))
      )
      val r8c =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            SP(?("K")),
            TQ(suffix, ?("K"), ?("S")),
            skolem(variables :+ ?("V") :+ ?("V") :+ ?("S"))
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
        Ans(?("K")),
        QM(?("K")),
        not(SP(?("K")))
      )

      (r1 :: r3a ::: r3b :: r3c :: r4a ::: r4b ::: r4c ::: r5a ::: r5b ::: r5c ::: r6 ::: r7b ::: r7a ::: r8a ::: r8b :: r8c ::: r9 :: List())
    }

  val answerQuery: String = {
    val arity = query.answer.size
    if (arity > 0) {
      val variables = (0 until arity).mkString("?X", " ?X", "")
      s"""
      SELECT ${query.answer mkString ""}
      WHERE {
          ?K a rsa:Ans .
          TT <http://oxfordsemantic.tech/RDFox#SKOLEM> { ${query.answer mkString ""} ${query.bounded mkString ""} ?K } .
      }
      """
    } else {
      "ASK { ?X a rsa:Ans }"
    }
  }

}
