package uk.ac.ox.cs.rsacomb

//import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Rule,
  TupleTableAtom,
  BodyFormula,
  Negation
}
import tech.oxfordsemantic.jrdfox.logic.expression.{Term, Variable}
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.suffix.{Forward, Backward}
import uk.ac.ox.cs.rsacomb.util.{RSA, RDFoxUtil}

/** Factory for [[uk.ac.ox.cs.rsacomb.FilteringProgram FilteringProgram]] */
object FilteringProgram {

  /** Create a new FilteringProgram instance.
    *
    * @param query CQ to be converted into logic rules.
    * @param constants constants in the original ontology. They will be
    * used to initialize predicate `rsa:Named`.
    */
  def apply(query: ConjunctiveQuery, constants: List[Term]): FilteringProgram =
    new FilteringProgram(query, constants)

}

/** Filtering Program generator
  *
  * Handles the conversion of a CQ into a set of logic rules,
  * representing the filtering step of the RSA combined approach.
  *
  * Instances can be created using the companion object.
  */
class FilteringProgram(query: ConjunctiveQuery, constants: List[Term]) {

  /** Extends capabilities of
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]]
    */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAtom._

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
    Rule.create(RSA.NI(varX), RSA.Congruent(varX, varY), RSA.Named(varY))

  /** Collection of filtering program rules. */
  val rules: List[Rule] =
    nis :: {

      /** Negates a [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]] */
      def not(atom: TupleTableAtom): BodyFormula = Negation.create(atom)

      /** Generates all possible, unfiltered answers.
        *
        * @note corresponds to rule 1 in Table 3 in the paper.
        */
      val r1 = Rule.create(RSA.QM, query.atoms: _*)

      /** Initializes instances of `rsa:Named`.
        *
        * They represent the set of constants appearing in the original
        * ontology.
        *
        * @note corresponds to rules 2 in Table 3.
        */
      val r2 = constants.map(c => Rule.create(RSA.Named(c)))

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
        role1 <- query.atoms filter (_.isRoleAssertion)
        index1 = query.bounded indexOf (role1.getArguments get 2)
        if index1 >= 0
        role2 <- query.atoms filter (_.isRoleAssertion)
        index2 = query.bounded indexOf (role2.getArguments get 2)
        if index2 >= 0
      } yield Rule.create(
        RSA.FK,
        role1 << Forward,
        role2 << Forward,
        RSA.ID(RSA(index1), RSA(index2)),
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
        RSA.FK,
        role1 << Forward,
        role2 << Backward,
        RSA.ID(RSA(index1), RSA(index2)),
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
        RSA.FK,
        role1 << Backward,
        role2 << Backward,
        RSA.ID(RSA(index1), RSA(index2)),
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
        RSA.ID(
          RSA(query.bounded indexOf r1arg0),
          RSA(query.bounded indexOf r2arg0)
        ),
        role1 << Forward,
        role2 << Forward,
        RSA.ID(
          RSA(query.bounded indexOf r1arg2),
          RSA(query.bounded indexOf r2arg2)
        ),
        RSA.Congruent(r1arg0, r2arg0),
        not(RSA.NI(r1arg0))
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
        RSA.ID(
          RSA(query.bounded indexOf r1arg0),
          RSA(query.bounded indexOf r2arg2)
        ),
        role1 << Forward,
        role2 << Backward,
        RSA.ID(
          RSA(query.bounded indexOf r1arg2),
          RSA(query.bounded indexOf r2arg0)
        ),
        RSA.Congruent(r1arg0, r2arg2),
        not(RSA.NI(r1arg0))
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
        RSA.ID(
          RSA(query.bounded indexOf r1arg2),
          RSA(query.bounded indexOf r2arg2)
        ),
        role1 << Backward,
        role2 << Backward,
        RSA.ID(
          RSA(query.bounded indexOf r1arg0),
          RSA(query.bounded indexOf r2arg0)
        ),
        RSA.Congruent(r1arg2, r2arg2),
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
        role <- query.atoms filter (_.isRoleAssertion)
        index0 = query.bounded indexOf (role.getArguments get 0)
        if index0 >= 0
        index2 = query.bounded indexOf (role.getArguments get 2)
        if index2 >= 0
        suffix <- Seq(Forward, Backward)
      } yield Rule.create(
        RSA.AQ(varV, varW, suffix),
        role << suffix,
        RSA.ID(RSA(index0), varV),
        RSA.ID(RSA(index2), varW)
      )
      val r7a =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            RSA.TQ(varU, varV, suffix),
            RSA.AQ(varU, varV, suffix)
          )
      val r7b =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            RSA.TQ(varU, varW, suffix),
            RSA.AQ(varU, varV, suffix),
            RSA.TQ(varV, varW, suffix)
          )

      /** Flag spurious answers.
        *
        * @note corresponds to rules 8x in Table 3.
        */
      val r8a =
        for (v <- query.answer)
          yield Rule.create(RSA.SP, RSA.QM, not(RSA.Named(v)))
      val r8b = Rule.create(RSA.SP, RSA.FK)
      val r8c =
        for (suffix <- List(Forward, Backward))
          yield Rule.create(
            RSA.SP,
            RSA.TQ(varV, varV, suffix)
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

      (r1 :: r2 :::
        r3a ::: r3b :: r3c ::
        r4a ::: r4b ::: r4c :::
        r5a ::: r5b ::: r5c :::
        r6 ::: r7b ::: r7a :::
        r8a ::: r8b :: r8c :::
        r9 :: List()) map RDFoxUtil.reify
    }

  /** Pretty-print filtering rule */
  override def toString(): String = rules mkString "\n"

}
