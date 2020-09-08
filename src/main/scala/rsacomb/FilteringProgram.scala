package rsacomb

import tech.oxfordsemantic.jrdfox.logic._
import scala.collection.JavaConverters._

class FilteringProgram(query: Query, constants: List[Term]) extends RDFTriple {

  private val bounded: List[Term] = this.getBoundedVariables
  private val answer: List[Term] = query.getAnswerVariables.asScala.toList

  val facts: List[Atom] = constants.map(named)
  val rules: List[Rule] = this.generateFilteringProgram()

  private def named(t: Term): Atom =
    Atom.rdf(t, IRI.RDF_TYPE, RSA.internal("NAMED"))

  private def getBoundedVariables: List[Variable] = List()

  private def generateFilteringProgram(): List[Rule] = {
    // Query formula as a rule body
    val body = queryToBody(query.getQueryFormula)
    // Auxiliar predicates/helpers
    def not(atom: Atom): BodyFormula = Negation.create(atom)
    val predQM =
      Atom.create(
        TupleTableName.create(RSA.internal("QM").getIRI),
        (answer ++ bounded): _*
      )
    def predID(t1: Term, t2: Term) =
      Atom.create(
        TupleTableName.create(RSA.internal("ID").getIRI),
        (answer ++ bounded).appended(t1).appended(t2): _*
      )
    def predNI(t1: Term): Atom =
      Atom.rdf(t1, IRI.RDF_TYPE, RSA.internal("NI"))
    def predTQ(sx: String, t1: Term, t2: Term) =
      Atom.create(
        TupleTableName.create(RSA.internal(s"TQ_$sx").getIRI),
        (answer ++ bounded).appended(t1).appended(t2): _*
      )
    def predAQ(sx: String, t1: Term, t2: Term) =
      Atom.create(
        TupleTableName.create(RSA.internal(s"AQ_$sx").getIRI),
        (answer ++ bounded).appended(t1).appended(t2): _*
      )
    val predFK =
      Atom.create(
        TupleTableName.create(RSA.internal("FK").getIRI),
        (answer ++ bounded): _*
      )
    val predSP =
      Atom.create(
        TupleTableName.create(RSA.internal("SP").getIRI),
        (answer ++ bounded): _*
      )
    val predANS =
      Atom.create(
        TupleTableName.create(RSA.internal("ANS").getIRI),
        answer: _*
      )

    /* Rule 1 */
    val r1 = Rule.create(predQM, body: _*)

    /* Rules 3x */
    val r3a =
      for ((v, i) <- bounded.zipWithIndex)
        yield Rule.create(
          predID(RSA.internal(i), RSA.internal(i)),
          predQM,
          not(predNI(v))
        )
    val r3b = Rule.create(
      predID(Variable.create("V"), Variable.create("U")),
      predID(Variable.create("U"), Variable.create("V"))
    )
    val r3c = Rule.create(
      predID(Variable.create("U"), Variable.create("W")),
      predID(Variable.create("U"), Variable.create("V")),
      predID(Variable.create("V"), Variable.create("W"))
    )

    /* Rules 4x */
    val r4a = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1 getArgument 2)
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2 getArgument 2)
    } yield Rule.create(
      predFK,
      role1 suffix "_f",
      role2 suffix "_f",
      predID(
        RSA.internal(bounded.indexOf(role1 getArgument 2)),
        RSA.internal(bounded.indexOf(role2 getArgument 2))
      ),
      not(Atom.rdf(role1 getArgument 0, IRI.SAME_AS, role2 getArgument 0))
    )
    val r4b = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1 getArgument 2)
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2 getArgument 0)
    } yield Rule.create(
      predFK,
      role1 suffix "_f",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded.indexOf(role1 getArgument 2)),
        RSA.internal(bounded.indexOf(role2 getArgument 0))
      ),
      not(Atom.rdf(role1 getArgument 0, IRI.SAME_AS, role2 getArgument 2))
    )
    val r4c = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1 getArgument 0)
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2 getArgument 0)
    } yield Rule.create(
      predFK,
      role1 suffix "_b",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded.indexOf(role1 getArgument 0)),
        RSA.internal(bounded.indexOf(role2 getArgument 0))
      ),
      not(Atom.rdf(role1 getArgument 2, IRI.SAME_AS, role2 getArgument 2))
    )

    /* Rules 5x */
    val r5a = for {
      role1 <- body.filter(_.isRoleAssertion)
      role1arg0 = role1 getArgument 0
      role1arg2 = role1 getArgument 2
      if bounded contains role1arg0
      if bounded contains role1arg2
      role2 <- body.filter(_.isRoleAssertion)
      role2arg0 = role2 getArgument 0
      role2arg2 = role2 getArgument 2
      if bounded contains role2arg0
      if bounded contains role2arg2
    } yield Rule.create(
      predID(
        RSA.internal(bounded indexOf role1arg0),
        RSA.internal(bounded indexOf role2arg0)
      ),
      role1 suffix "_f",
      role2 suffix "_f",
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg2)
      ),
      Atom.rdf(role1arg0, IRI.SAME_AS, role2arg0),
      not(predNI(role1arg0))
    )
    val r5b = for {
      role1 <- body.filter(_.isRoleAssertion)
      role1arg0 = role1 getArgument 0
      role1arg2 = role1 getArgument 2
      if bounded contains role1arg0
      if bounded contains role1arg2
      role2 <- body.filter(_.isRoleAssertion)
      role2arg0 = role2 getArgument 0
      role2arg2 = role2 getArgument 2
      if bounded contains role2arg0
      if bounded contains role2arg2
    } yield Rule.create(
      predID(
        RSA.internal(bounded indexOf role1arg0),
        RSA.internal(bounded indexOf role2arg2)
      ),
      role1 suffix "_f",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg0)
      ),
      Atom.rdf(role1arg0, IRI.SAME_AS, role2arg2),
      not(predNI(role1arg0))
    )
    val r5c = for {
      role1 <- body.filter(_.isRoleAssertion)
      role1arg0 = role1 getArgument 0
      role1arg2 = role1 getArgument 2
      if bounded contains role1arg0
      if bounded contains role1arg2
      role2 <- body.filter(_.isRoleAssertion)
      role2arg0 = role2 getArgument 0
      role2arg2 = role2 getArgument 2
      if bounded contains role2arg0
      if bounded contains role2arg2
    } yield Rule.create(
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg2)
      ),
      role1 suffix "_b",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded indexOf role1arg0),
        RSA.internal(bounded indexOf role2arg0)
      ),
      Atom.rdf(role1arg2, IRI.SAME_AS, role2arg2),
      not(predNI(role1arg2))
    )

    /* Rules 6 */
    val r6 = for {
      role <- body.filter(_.isRoleAssertion)
      arg0 = role getArgument 0
      arg2 = role getArgument 2
      if bounded contains arg0
      if bounded contains arg2
      sx <- List("_f", "_b")
    } yield Rule.create(
      predAQ(sx, Variable.create("V"), Variable.create("W")),
      role suffix sx,
      predID(
        RSA.internal(bounded indexOf arg0),
        RSA.internal(Variable.create("V"))
      ),
      predID(
        RSA.internal(bounded indexOf arg2),
        RSA.internal(Variable.create("W"))
      )
    )

    /* Rules 7x */
    val r7a =
      for (sx <- List("f", "b"))
        yield Rule.create(
          predTQ(sx, Variable.create("U"), Variable.create("V")),
          predAQ(sx, Variable.create("U"), Variable.create("V"))
        )
    val r7b =
      for (r <- List("f", "b"))
        yield Rule.create(
          predTQ(r, Variable.create("U"), Variable.create("W")),
          predAQ(r, Variable.create("U"), Variable.create("V")),
          predTQ(r, Variable.create("V"), Variable.create("W"))
        )

    /* Rules 8x */
    val r8a =
      for (v <- answer) yield Rule.create(predSP, predQM, not(named(v)))
    val r8b =
      Rule.create(predSP, predFK)
    val r8c =
      for (sx <- List("_f", "_b"))
        yield Rule.create(
          predSP,
          predTQ(sx, Variable.create("V"), Variable.create("V"))
        )

    /* Rule 9 */
    val r9 = Rule.create(predANS, predQM, not(predSP))

    List.empty
      .prepended(r9)
      .prependedAll(r8c)
      .prepended(r8b)
      .prependedAll(r8a)
      .prependedAll(r7b)
      .prependedAll(r7a)
      .prependedAll(r6)
      .prependedAll(r5c)
      .prependedAll(r5b)
      .prependedAll(r5a)
      .prependedAll(r4c)
      .prependedAll(r4b)
      .prependedAll(r4a)
      .prepended(r3c)
      .prepended(r3b)
      .prependedAll(r3a)
      .prepended(r1)
  }

  /* NOTE: we are restricting to queries that contain conjunctions of
   * atoms for the time being. This might need to be reviewed in the
   * future.
   */
  private def queryToBody(body: Formula): List[Atom] =
    body match {
      case a: Atom => List(a);
      case a: Conjunction =>
        a.getConjuncts.asScala.toList.flatMap(queryToBody);
      case _ => List()
    }

} // class FilteringProgram

object FilteringProgram {
  def apply(query: Query, constants: List[Term]): FilteringProgram =
    new FilteringProgram(query, constants)
}
