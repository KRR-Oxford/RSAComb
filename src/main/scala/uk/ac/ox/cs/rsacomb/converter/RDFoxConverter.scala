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

package uk.ac.ox.cs.rsacomb.converter

import java.util.stream.Collectors
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  BindAtom,
  BodyFormula,
  Rule,
  TupleTableAtom,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.expression.{Term, IRI, FunctionCall}
import uk.ac.ox.cs.rsacomb.RSAOntology
import uk.ac.ox.cs.rsacomb.suffix.{Empty, Inverse, RSASuffix}
import uk.ac.ox.cs.rsacomb.util.{DataFactory, RSA, RDFoxUtil}

/** Horn-ALCHOIQ to RDFox axiom converter.
  *
  * Provides the tools to translate Horn-ALCHOIQ axioms into logic rules
  * using RDFox syntax.
  *
  * @note the input axioms are assumed to be normalized. Trying to
  * convert non normalized axioms might result in undefined behavious.
  * We use the normalization defined in the main paper.
  */
trait RDFoxConverter {

  /** Simplify conversion between Java and Scala collections */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Simplify conversion between similar concepts in OWLAPI and RDFox
    * abstract syntax.
    */
  import uk.ac.ox.cs.rsacomb.implicits.RDFox._

  /** Factory used for converting expressions when needed.
    *
    * @note most of the time this is used to perform some kind of
    * normalization on axioms. In later versions it might be worth
    * moving the normalization code in its own independent step.
    */
  private val manager = OWLManager.createOWLOntologyManager()
  private val factory = manager.getOWLDataFactory()

  /** Default named graph to be used when generating new atoms */
  val graph: TupleTableName =
    TupleTableName.create("http://oxfordsemantic.tech/RDFox#DefaultTriples")

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

  /** Converts a [[OWLLogicalAxiom]] into a collection of [[TupleTableAtoms]] and [[Rules]].
    *
    * @note not all possible axioms are handled correctly, and in
    * general they are assumed to be normalised. Following is a list of
    * all unhandled class expressions:
    * - [[org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom OWLAsymmetricObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom OWLDatatypeDefinitionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom OWLDisjointDataPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom OWLDisjointObjectPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLHasKeyAxiom OWLHasKeyAxiom]]
    * - [[org.semanticweb.owlapi.model.SWRLRule SWRLRule]]
    *
    * @note The following axioms are not handled directly but can be
    * normalised beforehand.
    *
    * - [[org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom OWLTransitiveObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom OWLDataPropertyAssertionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom OWLDataPropertyRangeAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom OWLDifferentIndividualsAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom OWLReflexiveObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLSameIndividualAxiom OWLSameIndividualAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom OWLNegativeDataPropertyAssertionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom OWLNegativeObjectPropertyAssertionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom OWLIrreflexiveObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointUnionAxiom OWLDisjointUnionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom OWLEquivalentDataPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom OWLFunctionalDataPropertyAxiom]]
    *
    * @see [[Normaliser]]
    * @see
    *   http://owlcs.github.io/owlapi/apidocs_5/index.html
    */
  def convert(
      axiom: OWLLogicalAxiom,
      term: Term,
      unsafe: List[OWLObjectPropertyExpression],
      skolem: SkolemStrategy,
      suffix: RSASuffix
  )(implicit fresh: DataFactory): Result =
    axiom match {

      case a: OWLSubClassOfAxiom => {
        val subcls = a.getSubClass
        val supcls = a.getSuperClass
        val (sub, _) =
          convert(subcls, term, unsafe, NoSkolem, suffix)(fresh)
        val (sup, ext) =
          convert(supcls, term, unsafe, skolem, suffix)(fresh)
        val rule = Rule.create(sup, ext ::: sub)
        ResultR(List(rule))
      }

      // cannot be left
      // http://www.w3.org/TR/owl2-syntax/#Equivalent_Classes
      case a: OWLEquivalentClassesAxiom => {
        val (atoms, rules) = a.asPairwiseAxioms
          .flatMap(_.asOWLSubClassOfAxioms)
          .map(a => convert(a, term, unsafe, skolem dup a, suffix)(fresh))
          .unzip
        (atoms.flatten, rules.flatten)
      }

      case a: OWLEquivalentObjectPropertiesAxiom => {
        val (atoms, rules) = a.asPairwiseAxioms
          .flatMap(_.asSubObjectPropertyOfAxioms)
          .map(a => convert(a, term, unsafe, skolem dup a, suffix)(fresh))
          .unzip
        (atoms.flatten, rules.flatten)
      }

      case a: OWLSubObjectPropertyOfAxiom => {
        val term1 = fresh.getVariable
        val body = convert(a.getSubProperty, term, term1, suffix)(fresh)
        val head = convert(a.getSuperProperty, term, term1, suffix)(fresh)
        ResultR(List(Rule.create(head, body)))
      }

      case a: OWLSubDataPropertyOfAxiom => {
        val term1 = fresh.getVariable
        val body = convert(a.getSubProperty, term, term1, suffix)(fresh)
        val head = convert(a.getSuperProperty, term, term1, suffix)(fresh)
        ResultR(List(Rule.create(head, body)))
      }

      case a: OWLObjectPropertyDomainAxiom =>
        convert(a.asOWLSubClassOfAxiom, term, unsafe, skolem, suffix)(fresh)

      case a: OWLObjectPropertyRangeAxiom => {
        val term1 = fresh.getVariable
        val (res, ext) =
          convert(a.getRange, term, unsafe, skolem, suffix)(fresh)
        val prop = convert(a.getProperty, term1, term, suffix)(fresh)
        ResultR(List(Rule.create(res, prop :: ext)))
      }

      case a: OWLDataPropertyDomainAxiom =>
        convert(a.asOWLSubClassOfAxiom, term, unsafe, skolem, suffix)(fresh)

      case a: OWLDisjointClassesAxiom => {
        val body = a.getOperandsAsList.asScala.toSeq
          .flatMap((cls) =>
            convert(cls, term, unsafe, NoSkolem, suffix)(fresh)._1
          )
        val bottom =
          TupleTableAtom.create(graph, term, IRI.RDF_TYPE, IRI.NOTHING)
        ResultR(List(Rule.create(bottom, body: _*)))
      }

      case a: OWLInverseObjectPropertiesAxiom => {
        val (atoms, rules) = a.asSubObjectPropertyOfAxioms
          .map(a => convert(a, term, unsafe, skolem dup a, suffix)(fresh))
          .unzip
        (atoms.flatten, rules.flatten)
      }

      case a: OWLFunctionalObjectPropertyAxiom =>
        convert(a.asOWLSubClassOfAxiom, term, unsafe, skolem, suffix)(fresh)

      case a: OWLInverseFunctionalObjectPropertyAxiom =>
        convert(a.asOWLSubClassOfAxiom, term, unsafe, skolem, suffix)(fresh)

      case a: OWLSymmetricObjectPropertyAxiom => {
        val (atoms, rules) = a.asSubPropertyAxioms
          .map(a => convert(a, term, unsafe, skolem dup a, suffix)(fresh))
          .unzip
        (atoms.flatten, rules.flatten)
      }

      case a: OWLClassAssertionAxiom => {
        val ind = a.getIndividual
        ind match {
          case i: OWLNamedIndividual => {
            val cls = a.getClassExpression
            val (res, _) =
              convert(cls, i.getIRI, unsafe, NoSkolem, suffix)(fresh)
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
          val prop = convert(a.getProperty, subj, obj, suffix)(fresh)
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
          val prop = convert(a.getProperty, subj, obj, suffix)(fresh)
          ResultF(List(prop))
        }

      case a: OWLSubPropertyChainOfAxiom => {
        val (term1, body) =
          a.getPropertyChain.foldLeft((term, List[TupleTableAtom]())) {
            case ((term, atoms), prop) => {
              val term1 = fresh.getVariable
              val atom = convert(prop, term, term1, suffix)(fresh)
              (term1, atoms :+ atom)
            }
          }
        val head = convert(a.getSuperProperty, term, term1, suffix)(fresh)
        val rule = Rule.create(head, body)
        println(rule)
        ResultR(List(rule))
      }

      /** Catch-all case for all unhandled axiom types. */
      case a => unsupported(axiom)
    }

  protected def unsupported(axiom: OWLLogicalAxiom): Result =
    throw new RuntimeException(s"Axiom '$axiom' is not supported (yet?)")

  /** Converts a class expression into a collection of atoms.
    *
    * @note not all possible class expressions are handled correctly.
    * Following is a list of all unhandled class expressions:
    * - [[org.semanticweb.owlapi.model.OWLDataAllValuesFrom OWLDataAllValuesFrom]]
    * - [[org.semanticweb.owlapi.model.OWLDataExactCardinality OWLDataExactCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLDataMaxCardinality OWLDataMaxCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLObjectAllValuesFrom OWLObjectAllValuesFrom]]
    * - [[org.semanticweb.owlapi.model.OWLObjectComplementOf OWLObjectComplementOf]]
    * - [[org.semanticweb.owlapi.model.OWLObjectExactCardinality OWLObjectExactCardinality]]
    * - [[org.semanticweb.owlapi.model.OWLObjectHasSelf OWLObjectHasSelf]]
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
  )(implicit fresh: DataFactory): Shards =
    expr match {

      /** Simple class name.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Classes]]
        */
      case e: OWLClass => {
        val iri: IRI = if (e.isTopEntity()) IRI.THING else e.getIRI
        val atom = TupleTableAtom.create(graph, term, IRI.RDF_TYPE, iri)
        (List(atom), List())
      }

      /** Conjunction of class expressions.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Intersection_of_Class_Expressions]]
        */
      case e: OWLObjectIntersectionOf => {
        val (res, ext) = e.asConjunctSet
          .map(convert(_, term, unsafe, skolem, suffix)(fresh))
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
        val atom =
          TupleTableAtom.create(graph, term, RSA.CONGRUENT, named.head.getIRI)
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
        val varX = fresh.getVariable
        val (bind, term1) = skolem match {
          case NoSkolem    => (None, varX)
          case c: Constant => (None, c.iri)
          case s: Standard => (Some(RDFoxUtil.skolem(s.name, term, varX)), varX)
        }
        val (res, ext) = convert(cls, term1, unsafe, skolem, suffix)(fresh)
        val prop = convert(role, term, term1, suffix)(fresh)
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
        // Computes the result of rule skolemization. Depending on the used
        // technique it might involve the introduction of additional atoms,
        // and/or fresh constants and variables.
        val varX = fresh.getVariable
        val (bind, term1) = skolem match {
          case NoSkolem    => (None, varX)
          case c: Constant => (None, c.iri)
          case s: Standard => (Some(RDFoxUtil.skolem(s.name, term, varX)), varX)
        }
        val prop = convert(role, term, term1, suffix)(fresh)
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
          Seq(fresh.getVariable, fresh.getVariable)
        val cls = e.getFiller
        val role = e.getProperty
        val (res, ext) =
          vars.map(convert(cls, _, unsafe, skolem, suffix)(fresh)).unzip
        val props = vars.map(convert(role, term, _, suffix)(fresh))
        val eq = TupleTableAtom.create(graph, y, RSA.CONGRUENT, z)
        (List(eq), res.flatten ++ props)
      }

      /** Minimum cardinality restriction class
        *
        * @note we only admit classes with cardinality set to 1 because
        * they are equivalent to existential quantification.
        *
        * @throws `RuntimeException` when dealing with a restriction
        * with cardinality != 1.
        *
        * @see [[https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality]]
        */
      case e: OWLObjectMinCardinality => {
        if (e.getCardinality != 1)
          throw new RuntimeException(
            s"Class expression '$e' has cardinality restriction != 1."
          )
        val filler = e.getFiller
        val property = e.getProperty
        val expr = factory.getOWLObjectSomeValuesFrom(property, filler)
        convert(expr, term, unsafe, skolem, suffix)(fresh)
      }

      /** Minimum cardinality restriction class
        *
        * @note we only admit classes with cardinality set to 1 because
        * they are equivalent to existential quantification.
        *
        * @throws `RuntimeException` when dealing with a restriction
        * with cardinality != 1.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality_2]]
        */
      case e: OWLDataMinCardinality => {
        if (e.getCardinality != 1)
          throw new RuntimeException(
            s"Class expression '$e' has cardinality restriction != 1."
          )
        val filler = e.getFiller
        val property = e.getProperty
        val expr = factory.getOWLDataSomeValuesFrom(property, filler)
        convert(expr, term, unsafe, skolem, suffix)(fresh)
      }

      //case (_, sup: OWLObjectExactCardinality) => {
      //  println(s"Ignored: $a")
      //  return Result()
      //}

      //case (_, sup: OWLDataExactCardinality) => {
      //  println(s"Ignored: $a")
      //  return Result()
      //}

      /** Existential quantification with singleton filler
        *
        * @see
        * [[http://www.w3.org/TR/owl2-syntax/#Individual_Value_Restriction]]
        */
      case e: OWLObjectHasValue => {
        val term1: Term = e.getFiller match {
          case i: OWLNamedIndividual     => i.getIRI
          case i: OWLAnonymousIndividual => i.getID
        }
        (List(convert(e.getProperty, term, term1, suffix)(fresh)), List())
      }

      /** Existential quantification with singleton literal filler
        *
        * @see
        * [[http://www.w3.org/TR/owl2-syntax/#Literal_Value_Restriction]]
        */
      case e: OWLDataHasValue =>
        (List(convert(e.getProperty, term, e.getFiller, suffix)(fresh)), List())

      case e: OWLObjectUnionOf => {
        (List(), List())
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
  )(implicit fresh: DataFactory): TupleTableAtom =
    expr match {

      /** Simple named role/object property.
        *
        * @see [[http://www.w3.org/TR/owl2-syntax/#Object_Properties Object Properties]]
        */
      case e: OWLObjectProperty => {
        val role = IRI.create(e.getIRI.getIRIString :: suffix)
        TupleTableAtom.create(graph, term1, role, term2)
      }

      /** Inverse of a named role/property
        *
        * OWLAPI does not admit nesting of negation, and double
        * negations are always simplified.
        *
        * @see [[https://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties Inverse Object Properties]]
        */
      case e: OWLObjectInverseOf =>
        //convert(e.getInverse, term1, term2, suffix + Inverse)
        convert(e.getInverse, term2, term1, suffix)(fresh)

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
  )(implicit fresh: DataFactory): TupleTableAtom =
    expr match {

      /** Simple named role/data property
        *
        * @see [[https://www.w3.org/TR/owl2-syntax/#Datatypes Data Properties]]
        */
      case e: OWLDataProperty => {
        val role = IRI.create(e.getIRI.getIRIString :: suffix)
        TupleTableAtom.create(graph, term1, role, term2)
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
