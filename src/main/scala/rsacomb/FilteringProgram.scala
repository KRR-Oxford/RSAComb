package rsacomb

import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.expression.{
  Term,
  IRI,
  Variable,
  Literal,
  FunctionCall
}
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Rule,
  TupleTableAtom,
  BindAtom,
  TupleTableName,
  Atom,
  BodyFormula,
  Negation
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.{SelectQuery}
import tech.oxfordsemantic.jrdfox.logic.sparql.pattern.{
  GroupGraphPattern,
  ConjunctionPattern,
  TriplePattern,
  QueryPattern
}

import scala.collection.JavaConverters._

class FilteringProgram(query: SelectQuery, constants: List[Term])
    extends RDFTriple {

  /* Makes mplicit conversion OWLAPI IRI <-> RDFox IRI available */
  import RDFoxUtil._

  lazy val variables = {
    query.getQueryBody.getWherePattern match {
      case b: ConjunctionPattern => {
        b.getConjuncts.asScala.toSet.flatMap { conj: QueryPattern =>
          conj match {
            case c: TriplePattern =>
              Set(c.getSubject, c.getPredicate, c.getObject)
                .filter(_.isInstanceOf[Variable])
            case _ => Set()
          }
        }
      }
      case _ => Set()
    }
  }.toList

  val answer: List[Term] =
    if (query.getAllPossibleVariables) {
      variables
    } else {
      query.getSelection.asScala.map(_.getVariable).toList
    }
  val bounded: List[Term] = this.variables.filterNot(answer.contains(_))

  val facts: List[Rule] = constants.map(c => Rule.create(predNI(c)))
  val rules: List[Rule] =
    this.generateFilteringProgram().map(reifyRule) ++ facts

  private def predNI(t: Term): TupleTableAtom =
    TupleTableAtom.rdf(t, IRI.RDF_TYPE, RSA.internal("NI"))

  /* NOTE: we are restricting to queries that contain conjunctions of
   * atoms for the time being. This might need to be reviewed in the
   * future.
   */
  private def queryToBody(body: GroupGraphPattern): List[TupleTableAtom] =
    body match {
      case b: ConjunctionPattern => {
        val conjuncts = b.getConjuncts.asScala.toList
        conjuncts flatMap { conj =>
          conj match {
            case c: TriplePattern =>
              List(
                TupleTableAtom.rdf(c.getSubject, c.getPredicate, c.getObject)
              )
            case _ => List()
          }
        }
      }
      case _ => List()
    }

  private def generateFilteringProgram(): List[Rule] = {
    // Query formula as a rule body
    val body = queryToBody(query.getQueryBody.getWherePattern)
    // Auxiliar predicates/helpers
    def not(atom: TupleTableAtom): BodyFormula = Negation.create(atom)
    val predQM =
      TupleTableAtom.create(
        TupleTableName.create(RSA.internal("QM").getIRI),
        (answer ++ bounded): _*
      )
    def predID(t1: Term, t2: Term) =
      TupleTableAtom.create(
        TupleTableName.create(RSA.internal("ID").getIRI),
        (answer ++ bounded).appended(t1).appended(t2): _*
      )
    def predNAMED(t1: Term): TupleTableAtom =
      TupleTableAtom.rdf(t1, IRI.RDF_TYPE, RSA.internal("NAMED"))
    def predTQ(sx: String, t1: Term, t2: Term) =
      TupleTableAtom.create(
        TupleTableName.create(RSA.internal(s"TQ_$sx").getIRI),
        (answer ++ bounded).appended(t1).appended(t2): _*
      )
    def predAQ(sx: String, t1: Term, t2: Term) =
      TupleTableAtom.create(
        TupleTableName.create(RSA.internal(s"AQ_$sx").getIRI),
        (answer ++ bounded).appended(t1).appended(t2): _*
      )
    val predFK =
      TupleTableAtom.create(
        TupleTableName.create(RSA.internal("FK").getIRI),
        (answer ++ bounded): _*
      )
    val predSP =
      TupleTableAtom.create(
        TupleTableName.create(RSA.internal("SP").getIRI),
        (answer ++ bounded): _*
      )
    val predANS =
      TupleTableAtom.create(
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
      if bounded contains (role1.getArguments.get(2))
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2.getArguments.get(2))
    } yield Rule.create(
      predFK,
      role1 suffix "f",
      role2 suffix "f",
      predID(
        RSA.internal(bounded.indexOf(role1.getArguments.get(2))),
        RSA.internal(bounded.indexOf(role2.getArguments.get(2)))
      ),
      not(
        TupleTableAtom.rdf(
          role1.getArguments.get(0),
          RSA.EquivTo,
          role2.getArguments.get(0)
        )
      )
    )
    val r4b = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1.getArguments.get(2))
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2.getArguments.get(0))
    } yield Rule.create(
      predFK,
      role1 suffix "f",
      role2 suffix "b",
      predID(
        RSA.internal(bounded.indexOf(role1.getArguments.get(2))),
        RSA.internal(bounded.indexOf(role2.getArguments.get(0)))
      ),
      not(
        TupleTableAtom.rdf(
          role1.getArguments.get(0),
          RSA.EquivTo,
          role2.getArguments.get(2)
        )
      )
    )
    val r4c = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1.getArguments.get(0))
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2.getArguments.get(0))
    } yield Rule.create(
      predFK,
      role1 suffix "b",
      role2 suffix "b",
      predID(
        RSA.internal(bounded.indexOf(role1.getArguments.get(0))),
        RSA.internal(bounded.indexOf(role2.getArguments.get(0)))
      ),
      not(
        TupleTableAtom.rdf(
          role1.getArguments.get(2),
          RSA.EquivTo,
          role2.getArguments.get(2)
        )
      )
    )

    /* Rules 5x */
    val r5a = for {
      role1 <- body.filter(_.isRoleAssertion)
      role1arg0 = role1.getArguments.get(0)
      role1arg2 = role1.getArguments.get(2)
      if bounded contains role1arg0
      if bounded contains role1arg2
      role2 <- body.filter(_.isRoleAssertion)
      role2arg0 = role2.getArguments.get(0)
      role2arg2 = role2.getArguments.get(2)
      if bounded contains role2arg0
      if bounded contains role2arg2
    } yield Rule.create(
      predID(
        RSA.internal(bounded indexOf role1arg0),
        RSA.internal(bounded indexOf role2arg0)
      ),
      role1 suffix "f",
      role2 suffix "f",
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg2)
      ),
      TupleTableAtom.rdf(role1arg0, RSA.EquivTo, role2arg0),
      not(predNI(role1arg0))
    )
    val r5b = for {
      role1 <- body.filter(_.isRoleAssertion)
      role1arg0 = role1.getArguments.get(0)
      role1arg2 = role1.getArguments.get(2)
      if bounded contains role1arg0
      if bounded contains role1arg2
      role2 <- body.filter(_.isRoleAssertion)
      role2arg0 = role2.getArguments.get(0)
      role2arg2 = role2.getArguments.get(2)
      if bounded contains role2arg0
      if bounded contains role2arg2
    } yield Rule.create(
      predID(
        RSA.internal(bounded indexOf role1arg0),
        RSA.internal(bounded indexOf role2arg2)
      ),
      role1 suffix "f",
      role2 suffix "b",
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg0)
      ),
      TupleTableAtom.rdf(role1arg0, RSA.EquivTo, role2arg2),
      not(predNI(role1arg0))
    )
    val r5c = for {
      role1 <- body.filter(_.isRoleAssertion)
      role1arg0 = role1.getArguments.get(0)
      role1arg2 = role1.getArguments.get(2)
      if bounded contains role1arg0
      if bounded contains role1arg2
      role2 <- body.filter(_.isRoleAssertion)
      role2arg0 = role2.getArguments.get(0)
      role2arg2 = role2.getArguments.get(2)
      if bounded contains role2arg0
      if bounded contains role2arg2
    } yield Rule.create(
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg2)
      ),
      role1 suffix "b",
      role2 suffix "b",
      predID(
        RSA.internal(bounded indexOf role1arg0),
        RSA.internal(bounded indexOf role2arg0)
      ),
      TupleTableAtom.rdf(role1arg2, RSA.EquivTo, role2arg2),
      not(predNI(role1arg2))
    )

    /* Rules 6 */
    val r6 = for {
      role <- body.filter(_.isRoleAssertion)
      arg0 = role.getArguments.get(0)
      arg2 = role.getArguments.get(2)
      if bounded contains arg0
      if bounded contains arg2
      sx <- List("f", "b")
    } yield Rule.create(
      predAQ(sx, Variable.create("V"), Variable.create("W")),
      role suffix sx,
      predID(
        RSA.internal(bounded indexOf arg0),
        Variable.create("V")
      ),
      predID(
        RSA.internal(bounded indexOf arg2),
        Variable.create("W")
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
      for (v <- answer) yield Rule.create(predSP, predQM, not(predNAMED(v)))
    val r8b =
      Rule.create(predSP, predFK)
    val r8c =
      for (sx <- List("f", "b"))
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

  private def reifyTupleTableAtom(
      atom: TupleTableAtom
  ): (Option[BindAtom], List[TupleTableAtom]) = {
    if (!atom.isRdfTriple) {
      // Compute binding atom
      val bvar = RSA.getFreshVariable()
      val name =
        Literal.create(atom.getTupleTableName.toString(), Datatype.XSD_STRING)
      val args = atom.getArguments.asScala.toList.prepended(name)
      val bind = BindAtom.create(FunctionCall.create("SKOLEM", args: _*), bvar)
      // Compute reified atom
      def reifiedIRI(i: Int) = atom.getTupleTableName.getName ++ s"_$i"
      val atoms = atom.getArguments.asScala.toList.zipWithIndex
        .map { case (t, i) => TupleTableAtom.rdf(bvar, reifiedIRI(i), t) }
      (Some(bind), atoms)
    } else {
      (None, List(atom))
    }
  }

  private def reifyAtom(atom: Atom): (Option[BindAtom], List[Atom]) = {
    atom match {
      case tta: TupleTableAtom => reifyTupleTableAtom(tta)
      case other               => (None, List(other))
    }
  }

  private def reifyBodyFormula(formula: BodyFormula): List[BodyFormula] = {
    formula match {
      case atom: TupleTableAtom => reifyTupleTableAtom(atom)._2
      case neg: Negation => {
        val (bs, as) = neg.getNegatedAtoms.asScala.toList.map(reifyAtom).unzip
        val bind = bs.flatten.map(_.getBoundVariable).asJava
        val atoms = as.flatten.asJava
        List(Negation.create(bind, atoms))
      }
      case other => List(other)
    }
  }

  private def reifyRule(rule: Rule): Rule = {
    val (bs, hs) = rule.getHead.asScala.toList.map(reifyTupleTableAtom).unzip
    val head: List[TupleTableAtom] = hs.flatten
    val bind: List[BodyFormula] = bs.flatten
    val body: List[BodyFormula] =
      rule.getBody.asScala.toList.map(reifyBodyFormula).flatten
    Rule.create(head.asJava, (body ++ bind).asJava)
  }

} // class FilteringProgram

object FilteringProgram {
  def apply(query: SelectQuery, constants: List[Term]): FilteringProgram =
    new FilteringProgram(query, constants)
} // object FilteringProgram
