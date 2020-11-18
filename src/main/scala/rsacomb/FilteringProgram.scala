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

import implicits.RSAAtom
import suffix.{RSASuffix, Forward, Backward}
import util.RSA

class FilteringProgram(query: SelectQuery, constants: List[Term])
    extends RSAAtom {

  /* Makes mplicit conversion OWLAPI IRI <-> RDFox IRI available */
  // import implicits.RDFox._

  implicit val variables: (List[Term], List[Term]) = {
    val all: Set[Variable] = query.getQueryBody.getWherePattern match {
      case b: ConjunctionPattern => {
        b.getConjuncts.asScala.toSet.flatMap { conj: QueryPattern =>
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
    if (query.getAllPossibleVariables) {
      (all.toList, List())
    } else {
      val answer = query.getSelection.asScala.map(_.getVariable).toSet
      (answer.toList, (all &~ answer).toList)
    }
  }

  val (answer, bounded) = variables

  val facts: List[Rule] = constants.map(c => Rule.create(RSA.NI(c)))
  val rules: List[Rule] =
    this.generateFilteringProgram().map(reifyRule) ++ facts

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
    // General purpose variables
    val varU = Variable.create("U")
    val varV = Variable.create("V")
    val varW = Variable.create("W")
    // Query formula as a rule body
    val body = queryToBody(query.getQueryBody.getWherePattern)
    // Auxiliar predicates/helpers
    def not(atom: TupleTableAtom): BodyFormula = Negation.create(atom)
    // val predQM =
    //   TupleTableAtom.create(
    //     TupleTableName.create(RSA.Qm.getIRI),
    //     (answer ++ bounded): _*
    //   )
    // def predID(t1: Term, t2: Term) =
    //   TupleTableAtom.create(
    //     TupleTableName.create(RSA.rsa("ID").getIRI),
    //     (answer ++ bounded).appended(t1).appended(t2): _*
    //   )
    // def predNAMED(t1: Term): TupleTableAtom =
    //   TupleTableAtom.rdf(t1, IRI.RDF_TYPE, RSA.rsa("NAMED"))
    // def predTQ(t1: Term, t2: Term, sx: RSASuffix) =
    //   TupleTableAtom.create(
    //     TupleTableName.create(RSA.rsa("TQ" :: sx).getIRI),
    //     (answer ++ bounded).appended(t1).appended(t2): _*
    //   )
    // def predAQ(t1: Term, t2: Term, sx: RSASuffix) =
    //   TupleTableAtom.create(
    //     TupleTableName.create(RSA.rsa("AQ" :: sx).getIRI),
    //     (answer ++ bounded).appended(t1).appended(t2): _*
    //   )
    // val predFK =
    //   TupleTableAtom.create(
    //     TupleTableName.create(RSA.rsa("FK").getIRI),
    //     (answer ++ bounded): _*
    //   )
    // val predSP =
    //   TupleTableAtom.create(
    //     TupleTableName.create(RSA.rsa("SP").getIRI),
    //     (answer ++ bounded): _*
    //   )
    // val predANS =
    //   TupleTableAtom.create(
    //     TupleTableName.create(RSA.rsa("ANS").getIRI),
    //     answer: _*
    //   )

    /* Rule 1 */
    val r1 = Rule.create(RSA.QM, body: _*)

    /* Rules 3x */
    val r3a =
      for ((v, i) <- bounded.zipWithIndex)
        yield Rule.create(RSA.ID(RSA(i), RSA(i)), RSA.QM, not(RSA.NI(v)))
    val r3b = Rule.create(RSA.ID(varV, varU), RSA.ID(varU, varV))
    val r3c =
      Rule.create(RSA.ID(varU, varW), RSA.ID(varU, varV), RSA.ID(varV, varW))

    /* Rules 4x */
    val r4a = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1.getArguments.get(2))
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2.getArguments.get(2))
    } yield Rule.create(
      RSA.FK,
      role1 << Forward,
      role2 << Forward,
      RSA.ID(
        RSA(bounded.indexOf(role1.getArguments.get(2))),
        RSA(bounded.indexOf(role2.getArguments.get(2)))
      ),
      not(RSA.congruent(role1.getArguments.get(0), role2.getArguments.get(0)))
    )
    val r4b = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1.getArguments.get(2))
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2.getArguments.get(0))
    } yield Rule.create(
      RSA.FK,
      role1 << Forward,
      role2 << Backward,
      RSA.ID(
        RSA(bounded.indexOf(role1.getArguments.get(2))),
        RSA(bounded.indexOf(role2.getArguments.get(0)))
      ),
      not(RSA.congruent(role1.getArguments.get(0), role2.getArguments.get(2)))
    )
    val r4c = for {
      role1 <- body.filter(_.isRoleAssertion)
      if bounded contains (role1.getArguments.get(0))
      role2 <- body.filter(_.isRoleAssertion)
      if bounded contains (role2.getArguments.get(0))
    } yield Rule.create(
      RSA.FK,
      role1 << Backward,
      role2 << Backward,
      RSA.ID(
        RSA(bounded.indexOf(role1.getArguments.get(0))),
        RSA(bounded.indexOf(role2.getArguments.get(0)))
      ),
      not(RSA.congruent(role1.getArguments.get(2), role2.getArguments.get(2)))
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
      RSA.ID(
        RSA(bounded indexOf role1arg0),
        RSA(bounded indexOf role2arg0)
      ),
      role1 << Forward,
      role2 << Forward,
      RSA.ID(
        RSA(bounded indexOf role1arg2),
        RSA(bounded indexOf role2arg2)
      ),
      RSA.congruent(role1arg0, role2arg0),
      not(RSA.NI(role1arg0))
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
      RSA.ID(
        RSA(bounded indexOf role1arg0),
        RSA(bounded indexOf role2arg2)
      ),
      role1 << Forward,
      role2 << Backward,
      RSA.ID(
        RSA(bounded indexOf role1arg2),
        RSA(bounded indexOf role2arg0)
      ),
      RSA.congruent(role1arg0, role2arg2),
      not(RSA.NI(role1arg0))
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
      RSA.ID(
        RSA(bounded indexOf role1arg2),
        RSA(bounded indexOf role2arg2)
      ),
      role1 << Backward,
      role2 << Backward,
      RSA.ID(
        RSA(bounded indexOf role1arg0),
        RSA(bounded indexOf role2arg0)
      ),
      RSA.congruent(role1arg2, role2arg2),
      not(RSA.NI(role1arg2))
    )

    /* Rules 6 */
    val r6 = {
      for {
        role <- body.filter(_.isRoleAssertion)
        arg0 = role.getArguments.get(0)
        arg2 = role.getArguments.get(2)
        if bounded contains arg0
        if bounded contains arg2
        suffix <- Seq(Forward, Backward)
      } yield Rule.create(
        RSA.AQ(varV, varW, suffix),
        role << suffix,
        RSA.ID(RSA(bounded indexOf arg0), varV),
        RSA.ID(RSA(bounded indexOf arg2), varW)
      )
    }

    /* Rules 7x */
    val r7a = {
      for (suffix <- List(Forward, Backward))
        yield Rule.create(
          RSA.TQ(varU, varV, suffix),
          RSA.AQ(varU, varV, suffix)
        )
    }
    val r7b = {
      for (suffix <- List(Forward, Backward))
        yield Rule.create(
          RSA.TQ(varU, varW, suffix),
          RSA.AQ(varU, varV, suffix),
          RSA.TQ(varV, varW, suffix)
        )
    }

    /* Rules 8x */
    val r8a =
      for (v <- answer) yield Rule.create(RSA.SP, RSA.QM, not(RSA.Named(v)))
    val r8b =
      Rule.create(RSA.SP, RSA.FK)
    val r8c =
      for (suffix <- List(Forward, Backward))
        yield Rule.create(
          RSA.SP,
          RSA.TQ(varV, varV, suffix)
        )

    /* Rule 9 */
    val r9 = Rule.create(RSA.Ans, RSA.QM, not(RSA.SP))

    r1 ::
      r3a ::: r3b :: r3c ::
      r4c ::: r4b ::: r4a :::
      r5c ::: r5b ::: r5a :::
      r6 :::
      r7b ::: r7a :::
      r8a ::: r8b :: r8c :::
      r9 ::
      List()
  }

  private def reifyAtom(atom: Atom): (Option[BindAtom], List[Atom]) = {
    atom match {
      case atom: TupleTableAtom => atom.reified
      case other                => (None, List(other))
    }
  }

  private def reifyBodyFormula(formula: BodyFormula): List[BodyFormula] = {
    formula match {
      case atom: TupleTableAtom => atom.reified._2
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
    val (bs, hs) = rule.getHead.asScala.toList.map(_.reified).unzip
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
