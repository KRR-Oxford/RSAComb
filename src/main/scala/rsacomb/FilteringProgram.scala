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

  val facts: List[TupleTableAtom] = constants.map(named)
  val rules: List[Rule] = this.generateFilteringProgram().map(reifyRule)

  private def named(t: Term): TupleTableAtom =
    TupleTableAtom.rdf(
      t,
      IRI.RDF_TYPE,
      RSA.internal("NAMED")
    )

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
    def predNI(t1: Term): TupleTableAtom =
      TupleTableAtom.rdf(t1, IRI.RDF_TYPE, RSA.internal("NI"))
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
      role1 suffix "_f",
      role2 suffix "_f",
      predID(
        RSA.internal(bounded.indexOf(role1.getArguments.get(2))),
        RSA.internal(bounded.indexOf(role2.getArguments.get(2)))
      ),
      not(
        TupleTableAtom.rdf(
          role1.getArguments.get(0),
          IRI.SAME_AS,
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
      role1 suffix "_f",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded.indexOf(role1.getArguments.get(2))),
        RSA.internal(bounded.indexOf(role2.getArguments.get(0)))
      ),
      not(
        TupleTableAtom.rdf(
          role1.getArguments.get(0),
          IRI.SAME_AS,
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
      role1 suffix "_b",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded.indexOf(role1.getArguments.get(0))),
        RSA.internal(bounded.indexOf(role2.getArguments.get(0)))
      ),
      not(
        TupleTableAtom.rdf(
          role1.getArguments.get(2),
          IRI.SAME_AS,
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
      role1 suffix "_f",
      role2 suffix "_f",
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg2)
      ),
      TupleTableAtom.rdf(role1arg0, IRI.SAME_AS, role2arg0),
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
      role1 suffix "_f",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded indexOf role1arg2),
        RSA.internal(bounded indexOf role2arg0)
      ),
      TupleTableAtom.rdf(role1arg0, IRI.SAME_AS, role2arg2),
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
      role1 suffix "_b",
      role2 suffix "_b",
      predID(
        RSA.internal(bounded indexOf role1arg0),
        RSA.internal(bounded indexOf role2arg0)
      ),
      TupleTableAtom.rdf(role1arg2, IRI.SAME_AS, role2arg2),
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
      for (v <- answer) yield Rule.create(predSP, predQM, not(named(v)))
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

  private sealed trait Reified;
  private case class ReifiedHead(bind: BindAtom, atoms: List[TupleTableAtom])
      extends Reified
  private case class ReifiedBody(atoms: List[TupleTableAtom]) extends Reified
  private case class Unaltered(formula: BodyFormula) extends Reified

  private def getBindAtom(atom: TupleTableAtom): BindAtom = {
    val newvar = RSA.getFreshVariable()
    val name =
      Literal.create(atom.getTupleTableName.toString(), Datatype.XSD_STRING)
    val args = atom
      .getArguments()
      .asScala
      .toSeq
      .prepended(name) /* Unclear requirement for SKOLEM func calls */
    BindAtom.create(
      FunctionCall
        .create("SKOLEM", args: _*),
      newvar
    )
  }

  private def reifyAtom(
      atom: TupleTableAtom,
      variable: Variable
  ): List[TupleTableAtom] = {
    def iri(i: Int) = atom.getTupleTableName.getName ++ s"_$i"
    atom
      .getArguments()
      .asScala
      .zipWithIndex
      .map { case (t, i) => TupleTableAtom.rdf(variable, iri(i), t) }
      .toList
  }

  private def reifyBodyFormula(
      formula: BodyFormula,
      head: Boolean
  ): Reified = {
    formula match {
      case a: TupleTableAtom => {
        if (!a.isRdfTriple) {
          if (head) {
            val b = getBindAtom(a)
            ReifiedHead(b, reifyAtom(a, b.getBoundVariable))
          } else {
            ReifiedBody(reifyAtom(a, RSA.getFreshVariable))
          }
        } else {
          Unaltered(a)
        }
      }
      case a => Unaltered(a)
    }
  }

  private def reifyRule(rule: Rule): Rule = {
    // Rule body
    val body =
      rule.getBody.asScala.map(reifyBodyFormula(_, false)).flatMap {
        case ReifiedHead(_, _) => List(); /* handle impossible case */
        case ReifiedBody(x)    => x;
        case Unaltered(x)      => List(x)
      }
    // Rule head
    val reified = rule.getHead.asScala.map(reifyBodyFormula(_, true))
    val skols = reified.flatMap {
      case ReifiedHead(x, _) => Some(x);
      case ReifiedBody(_)    => None; /* handle impossible case */
      case Unaltered(_)      => None
    }
    val head = reified.flatMap {
      case ReifiedHead(_, x) => x;
      case ReifiedBody(_)    => List(); /* handle impossible case */
      case Unaltered(x) =>
        List(x.asInstanceOf[TupleTableAtom]) /* Can we do better that a cast? */
    }
    Rule.create(head.asJava, (skols ++ body).asJava)
  }

} // class FilteringProgram

object FilteringProgram {
  def apply(query: SelectQuery, constants: List[Term]): FilteringProgram =
    new FilteringProgram(query, constants)
} // object FilteringProgram
