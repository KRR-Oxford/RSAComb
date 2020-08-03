package rsacomb

import org.semanticweb.owlapi.model.{OWLAxiom, OWLSubClassOfAxiom, OWLEquivalentClassesAxiom}
import org.semanticweb.owlapi.model.OWLAxiomVisitorEx

import tech.oxfordsemantic.jrdfox.logic.{Rule, BodyFormula}
import tech.oxfordsemantic.jrdfox.logic.{Atom, Term, Variable, Literal}

import scala.collection.JavaConverters._

import rsacomb.SkolemStrategy
import rsacomb.RDFoxRuleShards
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom

object RDFoxAxiomConverter {

  def apply(term : Term, skolem : SkolemStrategy) : RDFoxAxiomConverter =
	new RDFoxAxiomConverter(term, skolem)

  def apply(term : Term) : RDFoxAxiomConverter =
	new RDFoxAxiomConverter(term, SkolemStrategy.None)

} // object RDFoxAxiomConverter

class RDFoxAxiomConverter(term : Term, skolem : SkolemStrategy)
  extends OWLAxiomVisitorEx[List[Rule]]
{

  override
  def visit(axiom : OWLSubClassOfAxiom) : List[Rule] = {
    // Skolemization is needed only for the head of an axiom
    val subVisitor = new RDFoxClassExprConverter(term,SkolemStrategy.None)
    val superVisitor = new RDFoxClassExprConverter(term, skolem)
    // Each visitor returns a `RDFoxRuleShards`, a tuple (res,ext):
    // - the `res` List is a list of atoms resulting from the conversion
    //   of the axiom.
    // - for some Class Expressions appearing in the head of an Axiom,
    //   the conversion might produce atoms that need to appear in the
    //   body (and not in the head) of the rule. This is what the `ext`
    //   List is for.
    val sub = axiom.getSubClass.accept(subVisitor)
    val sup = axiom.getSuperClass.accept(superVisitor)
    val head = sup.res.asJava
    val body = (sub.res ++ sup.ext).asJava
    List(Rule.create(head,body))
  }

  override
  def visit(axiom : OWLEquivalentClassesAxiom) : List[Rule] = {
    for {
      axiom1 <- axiom.asPairwiseAxioms.asScala.toList
      axiom2 <- axiom1.asOWLSubClassOfAxioms.asScala.toList
      rule   <- axiom2.accept(this)
    } yield rule
  }

  override
  def visit(axiom : OWLSubObjectPropertyOfAxiom) : List[Rule] = {
    // TODO: variables needs to be handled at visitor level. Hardcoding
    // the name of the varibles might lead to errors for complex cases.
    val term1 = Variable.create("y")
    val subVisitor = new RDFoxPropertyExprConverter(term,term1,SkolemStrategy.None)
    val superVisitor = new RDFoxPropertyExprConverter(term,term1,skolem)
    val body: List[BodyFormula] = axiom.getSubProperty.accept(subVisitor)
    val head: List[Atom] = axiom.getSuperProperty.accept(superVisitor)
    List(Rule.create(head.asJava,body.asJava))
  }

  def doDefault(axiom : OWLAxiom) : List[Rule] = List()

} // class RDFoxAxiomConverter
