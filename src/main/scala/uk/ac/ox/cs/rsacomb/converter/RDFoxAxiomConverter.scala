package uk.ac.ox.cs.rsacomb.converter

import org.semanticweb.owlapi.model.{
  OWLAxiom,
  OWLSubClassOfAxiom,
  OWLEquivalentClassesAxiom,
  OWLObjectPropertyExpression
}
import org.semanticweb.owlapi.model.OWLAxiomVisitorEx

import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Rule,
  BodyFormula,
  TupleTableAtom,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  Term,
  IRI,
  Variable,
  Literal
}

import scala.collection.JavaConverters._

import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom

import uk.ac.ox.cs.rsacomb.RSAOntology
import uk.ac.ox.cs.rsacomb.suffix.{RSASuffix, Empty}

object RDFoxAxiomConverter {

  def apply(
      term: Term,
      unsafe: List[OWLObjectPropertyExpression],
      skolem: SkolemStrategy = SkolemStrategy.None,
      suffix: RSASuffix = Empty
  ): RDFoxAxiomConverter =
    new RDFoxAxiomConverter(term, unsafe, skolem, suffix)

} // object RDFoxAxiomConverter

class RDFoxAxiomConverter(
    term: Term,
    unsafe: List[OWLObjectPropertyExpression],
    skolem: SkolemStrategy,
    suffix: RSASuffix
) extends OWLAxiomVisitorEx[List[Rule]] {

  override def visit(axiom: OWLSubClassOfAxiom): List[Rule] = {
    // Skolemization is needed only for the head of an axiom
    val subVisitor =
      new RDFoxClassExprConverter(term, unsafe, SkolemStrategy.None, suffix)
    val superVisitor = new RDFoxClassExprConverter(term, unsafe, skolem, suffix)
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
    List(Rule.create(head, body))
  }

  override def visit(axiom: OWLEquivalentClassesAxiom): List[Rule] = {
    for {
      axiom1 <- axiom.asPairwiseAxioms.asScala.toList
      axiom2 <- axiom1.asOWLSubClassOfAxioms.asScala.toList
      rule <- axiom2.accept(this)
    } yield rule
  }

  override def visit(axiom: OWLSubObjectPropertyOfAxiom): List[Rule] = {
    val term1 = RSAOntology.genFreshVariable()
    val subVisitor =
      new RDFoxPropertyExprConverter(term, term1, suffix)
    val superVisitor = new RDFoxPropertyExprConverter(term, term1, suffix)
    val body: List[BodyFormula] = axiom.getSubProperty.accept(subVisitor)
    val head: List[TupleTableAtom] = axiom.getSuperProperty.accept(superVisitor)
    List(Rule.create(head.asJava, body.asJava))
  }

  override def visit(axiom: OWLClassAssertionAxiom): List[Rule] = {
    val ind = axiom.getIndividual
    if (ind.isNamed) {
      val term = IRI.create(ind.asOWLNamedIndividual().getIRI.getIRIString)
      val cls = axiom.getClassExpression
      val visitor =
        new RDFoxClassExprConverter(term, unsafe, SkolemStrategy.None, suffix)
      val shard = cls.accept(visitor)
      List(Rule.create(shard.res.asJava, shard.ext.asJava))
    } else {
      List()
    }
  }

  def doDefault(axiom: OWLAxiom): List[Rule] = List()

} // class RDFoxAxiomConverter
