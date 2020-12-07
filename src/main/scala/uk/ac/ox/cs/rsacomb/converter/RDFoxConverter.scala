package uk.ac.ox.cs.rsacomb.converter

import java.util.stream.Collectors
import org.semanticweb.owlapi.model.{
  OWLAnnotationProperty,
  OWLLogicalAxiom,
  OWLClass,
  OWLClassAssertionAxiom,
  OWLClassExpression,
  OWLDataProperty,
  OWLDataPropertyAssertionAxiom,
  OWLDataPropertyDomainAxiom,
  OWLDataPropertyExpression,
  OWLDataSomeValuesFrom,
  OWLEquivalentClassesAxiom,
  OWLEquivalentObjectPropertiesAxiom,
  OWLInverseObjectPropertiesAxiom,
  OWLNamedIndividual,
  OWLObjectIntersectionOf,
  OWLObjectInverseOf,
  OWLObjectMaxCardinality,
  OWLObjectOneOf,
  OWLObjectProperty,
  OWLObjectPropertyAssertionAxiom,
  OWLObjectPropertyDomainAxiom,
  OWLObjectPropertyExpression,
  OWLObjectPropertyRangeAxiom,
  OWLObjectSomeValuesFrom,
  OWLPropertyExpression,
  OWLSubClassOfAxiom,
  OWLSubObjectPropertyOfAxiom
}
import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  BindAtom,
  BodyFormula,
  Rule,
  TupleTableAtom
}
import tech.oxfordsemantic.jrdfox.logic.expression.{Term, IRI, FunctionCall}
import uk.ac.ox.cs.rsacomb.RSAOntology
import uk.ac.ox.cs.rsacomb.suffix.{Empty, Inverse, RSASuffix}
import uk.ac.ox.cs.rsacomb.util.RSA

/** Horn-ALCHOIQ to RDFox axiom converter.
  *
  * Provides the tools to translate Horn-ALCHOIQ axioms into logic rules
  * using RDFox syntax.
  *
  * @note the input axioms are assumed to be normalized. Trying to
  * convert non normalized axioms might result in undefined behavious.
  * We use the normalization defined in the main paper.
  *
  * @see [[https://github.com/KRR-Oxford/RSA-combined-approach GitHub repository]]
  * for more information on the theoretical aspects of the system.
  *
  * @todo this is not ideal and it would be more sensible to prepend a
  * normalization procedure that will prevent errors or unexpected
  * results.
  */
trait RDFoxConverter {

  /** Simplify conversion between Java and Scala collections */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Simplify conversion between similar concepts in OWLAPI and RDFox
    * abstract syntax.
    */
  import uk.ac.ox.cs.rsacomb.implicits.RDFox._

  /** Represents the result of the conversion of a
    * [[org.semanticweb.owlapi.model.OWLClassExpression OWLClassExpression]].
    *
    * In general a class expression is translated into a list of
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtoms]].
    * In some cases a class appearing on the right of a GCI might
    * generate additional atoms that will appear in the body of the
    * resulting formula.
    *
    * @example
    * In `A ⊑ ≤1R.B`, translated as
    * ```
    *   y = z <- A(x), R(x,y), B(y), R(x,z), B(z)
    * ```
    * the atom `≤1R.B` produces `y = z` to appear as head of the rule,
    * along with a set of atoms for the body of the rule (namely
    * `R(x,y), B(y), R(x,z), B(z)`).
    */
  protected type Shards = (List[TupleTableAtom], List[BodyFormula])

  /** Represent the result of the conversion of
    * [[org.semanticweb.owlapi.model.OWLLogicalAxiom OWLLogicalAxiom]].
    *
    * In general we have assertion returning (a collection of) atoms,
    * while other axioms that generate rules.
    */
  protected type Result = (List[TupleTableAtom], List[Rule])
  protected def Result(): Result = (List(), List())
  protected def ResultF(atoms: List[TupleTableAtom]): Result = (atoms, List())
  protected def ResultR(rules: List[Rule]): Result = (List(), rules)

  /** Converts a
    * [[org.semanticweb.owlapi.model.OWLLogicalAxiom OWLLogicalAxiom]]
    * into a collection of
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtoms]]
    * and
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.Rule Rules]].
    *
    * @note not all possible axioms are handled correctly, and in
    * general they are assumed to be normalised. Following is a list of
    * all unhandled class expressions:
    * - [[org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom OWLAsymmetricObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom OWLDataPropertyAssertionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom OWLDataPropertyRangeAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom OWLDatatypeDefinitionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom OWLDifferentIndividualsAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointClassesAxiom OWLDisjointClassesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom OWLDisjointDataPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom OWLDisjointObjectPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointUnionAxiom OWLDisjointUnionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom OWLEquivalentDataPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom OWLFunctionalDataPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom OWLFunctionalObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLHasKeyAxiom OWLHasKeyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom OWLInverseFunctionalObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom OWLIrreflexiveObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom OWLNegativeDataPropertyAssertionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom OWLNegativeObjectPropertyAssertionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom OWLReflexiveObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLSameIndividualAxiom OWLSameIndividualAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom OWLSubDataPropertyOfAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom OWLSubPropertyChainOfAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom OWLSymmetricObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom OWLTransitiveObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.SWRLRule SWRLRule]]
    */
  def convert(
      axiom: OWLLogicalAxiom,
      term: Term,
      unsafe: List[OWLObjectPropertyExpression],
      skolem: SkolemStrategy,
      suffix: RSASuffix
  ): Result =
    axiom match {

      case a: OWLSubClassOfAxiom => {
        val (sub, _) =
          convert(a.getSubClass, term, unsafe, NoSkolem, suffix)
        val (sup, ext) =
          convert(a.getSuperClass, term, unsafe, skolem, suffix)
        val rule = Rule.create(sup, ext ::: sub)
        ResultR(List(rule))
      }

      // cannot be left
      // http://www.w3.org/TR/owl2-syntax/#Equivalent_Classes
      case a: OWLEquivalentClassesAxiom => {
        val (atoms, rules) = a.asPairwiseAxioms
          .flatMap(_.asOWLSubClassOfAxioms)
          .map(a => convert(a, term, unsafe, skolem dup a, suffix))
          .unzip
        (atoms.flatten, rules.flatten)
      }

      case a: OWLEquivalentObjectPropertiesAxiom => {
        val (atoms, rules) = a.asPairwiseAxioms
          .flatMap(_.asSubObjectPropertyOfAxioms)
          .map(a => convert(a, term, unsafe, skolem dup a, suffix))
          .unzip
        (atoms.flatten, rules.flatten)
      }

      case a: OWLSubObjectPropertyOfAxiom => {
        val term1 = RSAOntology.genFreshVariable()
        val body = convert(a.getSubProperty, term, term1, suffix)
        val head = convert(a.getSuperProperty, term, term1, suffix)
        ResultR(List(Rule.create(head, body)))
      }

      case a: OWLObjectPropertyDomainAxiom =>
        convert(a.asOWLSubClassOfAxiom, term, unsafe, skolem, suffix)

      case a: OWLObjectPropertyRangeAxiom => {
        val term1 = RSAOntology.genFreshVariable()
        val (res, ext) = convert(a.getRange, term, unsafe, skolem, suffix)
        val prop = convert(a.getProperty, term1, term, suffix)
        ResultR(List(Rule.create(res, prop :: ext)))
      }

      case a: OWLDataPropertyDomainAxiom =>
        convert(a.asOWLSubClassOfAxiom, term, unsafe, skolem, suffix)

      case a: OWLInverseObjectPropertiesAxiom => {
        val (atoms, rules) = a.asSubObjectPropertyOfAxioms
          .map(a => convert(a, term, unsafe, skolem dup a, suffix))
          .unzip
        (atoms.flatten, rules.flatten)
      }

      case a: OWLClassAssertionAxiom => {
        val ind = a.getIndividual
        ind match {
          case i: OWLNamedIndividual => {
            val cls = a.getClassExpression
            val (res, _) =
              convert(cls, i.getIRI, unsafe, NoSkolem, suffix)
            ResultF(res)
          }
          case _ => Result()
        }
      }

      case a: OWLObjectPropertyAssertionAxiom =>
        if (!a.getSubject.isNamed || !a.getObject.isNamed)
          Result()
        else {
          val subj = a.getSubject.asOWLNamedIndividual.getIRI
          val obj = a.getObject.asOWLNamedIndividual.getIRI
          val prop = convert(a.getProperty, subj, obj, suffix)
          ResultF(List(prop))
        }

      /** Data property assertion.
        *
        * @see [[org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom OWLDataPropertyAssertionAxiom]]
        */
      case a: OWLDataPropertyAssertionAxiom =>
        if (!a.getSubject.isNamed)
          Result()
        else {
          val subj = a.getSubject.asOWLNamedIndividual.getIRI
          val obj = a.getObject
          val prop = convert(a.getProperty, subj, obj, suffix)
          ResultF(List(prop))
        }

      /** Catch-all case for all unhandled axiom types. */
      case a =>
        throw new RuntimeException(
          s"Axiom '$a' is not supported (yet?)"
        )

    }

  /** Converts a class expression into a collection of atoms.
    *
    * @note not all possible class expressions are handled correctly.
    * Following is a list of all unhandled class expressions:
    * - [[org.semanticweb.owlapi.model.OWLDataAllValuesFrom OWLDataAllValuesFrom]]
    * - [[org.semanticweb.owlapi.model.OWLDataExactCardinality OWLDataExactCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLDataMaxCardinality OWLDataMaxCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLDataMinCardinality OWLDataMinCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLDataHasValue OWLDataHasValue]]
    * - [[org.semanticweb.owlapi.model.OWLObjectAllValuesFrom OWLObjectAllValuesFrom]]
    * - [[org.semanticweb.owlapi.model.OWLObjectComplementOf OWLObjectComplementOf]]
    * - [[org.semanticweb.owlapi.model.OWLObjectExactCardinality OWLObjectExactCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLObjectHasSelf OWLObjectHasSelf]]
    * - [[org.semanticweb.owlapi.model.OWLObjectHasValue OWLObjectHasValue]]
    * - [[org.semanticweb.owlapi.model.OWLObjectMinCardinality OWLObjectMinCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLObjectUnionOf OWLObjectUnionOf]]
    *
    * Moreover:
    * - [[org.semanticweb.owlapi.model.OWLObjectMaxCardinality OWLObjectMaxCardinality]]
    *   is accepted only when cardinality is set to 1;
    * - [[org.semanticweb.owlapi.model.OWLObjectOneOf OWLObjectOneOf]]
    *   is accepted only when its arity is 1.
    */
  def convert(
      expr: OWLClassExpression,
      term: Term,
      unsafe: List[OWLObjectPropertyExpression],
      skolem: SkolemStrategy,
      suffix: RSASuffix
  ): Shards =
    expr match {

      /** Simple class name.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Classes]]
        */
      case e: OWLClass => {
        val iri: IRI = if (e.isTopEntity()) IRI.THING else e.getIRI
        val atom = TupleTableAtom.rdf(term, IRI.RDF_TYPE, iri)
        (List(atom), List())
      }

      /** Conjunction of class expressions.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Intersection_of_Class_Expressions]]
        */
      case e: OWLObjectIntersectionOf => {
        val (res, ext) = e.asConjunctSet
          .map(convert(_, term, unsafe, skolem, suffix))
          .unzip
        (res.flatten, ext.flatten)
      }

      /** Enumeration of individuals.
        *
        * @note we only admit enumerations of arity 1.
        *
        * @throws `RuntimeException` when dealing with an enumeration
        * with arity != 1.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Enumeration_of_Individuals]]
        */
      case e: OWLObjectOneOf => {
        val named = e.individuals
          .collect(Collectors.toList())
          .collect { case x: OWLNamedIndividual => x }
        if (named.length != 1)
          throw new RuntimeException(s"Class expression '$e' has arity != 1.")
        val atom = TupleTableAtom.rdf(term, IRI.SAME_AS, named.head.getIRI)
        (List(atom), List())
      }

      /** Existential class expression (for data properties).
        *
        * Parameter `skolem` is used to determine the skolemization
        * technique (if any) to use for the translation.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Existential_Quantification]]
        */
      case e: OWLObjectSomeValuesFrom => {
        val cls = e.getFiller()
        val role = e.getProperty()
        val varX = RSAOntology.genFreshVariable
        val (bind, term1) = skolem match {
          case NoSkolem    => (None, varX)
          case c: Constant => (None, c.iri)
          case s: Standard => {
            val func = FunctionCall.create("SKOLEM", s.literal, term)
            (Some(BindAtom.create(func, varX)), varX)
          }
        }
        val (res, ext) = convert(cls, term1, unsafe, skolem, suffix)
        val prop = convert(role, term, term1, suffix)
        (prop :: res, ext ++ bind)
      }

      /** Existential class expression (for data properties).
        *
        * Parameter `skolem` is used to determine the skolemization
        * technique (if any) to use for the translation.
        *
        * @todo the "filler" of this OWL expression is currently ignored.
        * This, in general might not be how we want to handle
        * [[org.semanticweb.owlapi.model.OWLDataRange OWLDataRanges]].
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Existential_Quantification_2]]
        */
      case e: OWLDataSomeValuesFrom => {
        val role = e.getProperty()
        // TODO: simplify this:
        // Computes the result of rule skolemization. Depending on the used
        // technique it might involve the introduction of additional atoms,
        // and/or fresh constants and variables.
        val varX = RSAOntology.genFreshVariable
        val (bind, term1) = skolem match {
          case NoSkolem    => (None, varX)
          case c: Constant => (None, c.iri)
          case s: Standard => {
            val func = FunctionCall.create("SKOLEM", s.literal, term)
            (Some(BindAtom.create(func, varX)), varX)
          }
        }
        val prop = convert(role, term, term1, suffix)
        (List(prop), bind.toList)
      }

      /** Maximum cardinality restriction class
        *
        * @note we only admit classes with cardinality set to 1.
        *
        * @throws `RuntimeException` when dealing with a restriction
        * with cardinality != 1.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality_2]]
        */
      case e: OWLObjectMaxCardinality => {
        if (e.getCardinality != 1)
          throw new RuntimeException(
            s"Class expression '$e' has cardinality restriction != 1."
          )
        val vars @ (y :: z :: _) =
          Seq(RSAOntology.genFreshVariable(), RSAOntology.genFreshVariable())
        val cls = e.getFiller
        val role = e.getProperty
        val (res, ext) = vars.map(convert(cls, _, unsafe, skolem, suffix)).unzip
        val props = vars.map(convert(role, term, _, suffix))
        val eq = TupleTableAtom.rdf(y, IRI.SAME_AS, z)
        (List(eq), res.flatten ++ props)
      }

      /** Catch-all case for all unhandled class expressions. */
      case e =>
        throw new RuntimeException(
          s"Class expression '$e' is not supported (yet?)"
        )
    }

  /** Converts an object property expression into an atom. */
  def convert(
      expr: OWLObjectPropertyExpression,
      term1: Term,
      term2: Term,
      suffix: RSASuffix
  ): TupleTableAtom =
    expr match {

      /** Simple named role/object property.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Object_Properties Object Properties]]
        */
      case e: OWLObjectProperty => {
        val role = IRI.create(e.getIRI.getIRIString :: suffix)
        TupleTableAtom.rdf(term1, role, term2)
      }

      /** Inverse of a named role/property
        *
        * OWLAPI does not admit nesting of negation, and double
        * negations are always simplified.
        *
        * @see [[https://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties Inverse Object Properties]]
        */
      case e: OWLObjectInverseOf =>
        convert(e.getInverse, term1, term2, suffix + Inverse)

      /** The infamous impossible case.
        *
        * @note all relevant cases are taken care of, and this branch
        * throws a runtime exception to notify of the problem.
        */
      case e =>
        throw new RuntimeException(
          s"Unable to convert '$e' into a logic expression. This should be happening (TM)."
        )
    }

  /** Converts a data property expression into an atom. */
  def convert(
      expr: OWLDataPropertyExpression,
      term1: Term,
      term2: Term,
      suffix: RSASuffix
  ): TupleTableAtom =
    expr match {

      /** Simple named role/data property
        *
        * @see [[https://www.w3.org/TR/owl2-syntax/#Datatypes Data Properties]]
        */
      case e: OWLDataProperty => {
        val role = IRI.create(e.getIRI.getIRIString :: suffix)
        TupleTableAtom.rdf(term1, role, term2)
      }

      /** The infamous impossible case.
        *
        * @note all relevant cases are taken care of, and this branch
        * throws a runtime exception to notify of the problem.
        */
      case e =>
        throw new RuntimeException(
          s"Unable to convert '$e' into a logic expression. This should be happening (TM)."
        )
    }

}
