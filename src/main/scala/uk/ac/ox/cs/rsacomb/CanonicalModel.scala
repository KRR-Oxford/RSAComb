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

package uk.ac.ox.cs.rsacomb

import org.semanticweb.owlapi.model.{OWLObjectInverseOf, OWLObjectProperty}
import org.semanticweb.owlapi.model.{
  OWLClass,
  OWLLogicalAxiom,
  // OWLObjectProperty,
  OWLSubObjectPropertyOfAxiom,
  OWLObjectPropertyExpression,
  OWLObjectSomeValuesFrom,
  OWLSubClassOfAxiom
}

import tech.oxfordsemantic.jrdfox.logic.datalog.{
  BodyFormula,
  Negation,
  Rule,
  TupleTableAtom,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term, Variable}

import implicits.JavaCollections._

import uk.ac.ox.cs.rsacomb.converter._
import uk.ac.ox.cs.rsacomb.suffix._
import uk.ac.ox.cs.rsacomb.util.{DataFactory, RSA}

/** Canonical model generator
  *
  * Converts the input axioms in a given ontology into logic rules that
  * can then be passed to RDFox to compute the actual canonical model
  * (via materialization).
  *
  * @param ontology the RSA ontology the canonical model is targeting.
  * @param graph the graph the canonical model will be generated into.
  */
class CanonicalModel(val ontology: RSAOntology, val graph: IRI) {

  /** Simplify conversion between OWLAPI and RDFox concepts */
  import implicits.RDFox._

  /** Extends capabilities of
    * [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]]
    */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._

  /** Introduce additional rules for each role.
    *
    * Some relations between roles and their inverse or their "suffixed"
    * versions need to be explicitly stated in terms of logic rules.
    */
  val rolesAdditionalRules: List[Rule] = {
    val tt = TupleTableName.create(graph.getIRI)
    ontology.objroles
      .collect { case prop: OWLObjectProperty => prop }
      .flatMap((pred) => {
        val iri = pred.getIRI.getIRIString
        val (varX, varY) = (Variable.create("X"), Variable.create("Y"))
        for (
          (hSuffix, bSuffix) <- Seq(
            (Empty, Forward),
            (Empty, Backward),
            (Inverse, Forward + Inverse),
            (Inverse, Backward + Inverse),
            (Backward + Inverse, Forward),
            (Forward + Inverse, Backward),
            (Backward, Forward + Inverse),
            (Forward, Backward + Inverse)
          )
        )
          yield Rule.create(
            TupleTableAtom.create(tt, varX, iri :: hSuffix, varY),
            TupleTableAtom.create(tt, varX, iri :: bSuffix, varY)
          )
      })
  }

  val (facts, rules): (List[TupleTableAtom], List[Rule]) = {
    // Compute rules from ontology axioms
    val (facts, rules) = {
      val term = Variable.create("X")
      val unsafe = ontology.unsafe
      ontology.axioms
        .map(a =>
          CanonicalModelConverter.convert(a, term, unsafe, Constant(a), Empty)
        )
        .unzip
    }
    (
      facts.flatten,
      rolesAdditionalRules ::: rules.flatten
    )
  }

  object CanonicalModelConverter extends RDFoxConverter {

    override val graph = TupleTableName.create(CanonicalModel.this.graph.getIRI)

    private def rules1(
        axiom: OWLSubClassOfAxiom
    ): Result = {
      val unfold = ontology.unfold(axiom).toList
      // Fresh Variables
      val v0 = RSA("v0_" ++ axiom.hashed)
      val varX = Variable.create("X")
      // TODO: use axiom.toTriple instead
      val atomA: TupleTableAtom = {
        val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
        TupleTableAtom.create(graph, varX, IRI.RDF_TYPE, cls)
      }
      val roleRf: TupleTableAtom = {
        val prop =
          axiom.getSuperClass.asInstanceOf[OWLObjectSomeValuesFrom].getProperty
        super.convert(prop, varX, v0, Forward)
      }
      val atomB: TupleTableAtom = {
        val cls = axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getFiller
          .asInstanceOf[OWLClass]
          .getIRI
        TupleTableAtom.create(graph, v0, IRI.RDF_TYPE, cls)
      }
      val unfoldSet = RSA(unfold.hashCode.toString)
      val facts = unfold.map(TupleTableAtom.create(graph, _, RSA.IN, unfoldSet))
      val notInX =
        Negation.create(TupleTableAtom.create(graph, varX, RSA.IN, unfoldSet))
      val rules = List(
        Rule.create(roleRf, atomA, notInX),
        Rule.create(atomB, atomA, notInX)
      )
      (facts, rules)
    }

    private def rules2(axiom: OWLSubClassOfAxiom): List[Rule] = {
      val roleR =
        axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getProperty
      if (ontology.confl(roleR) contains roleR) {
        // Fresh Variables
        val v0 = RSA("v0_" ++ axiom.hashed)
        val v1 = RSA("v1_" ++ axiom.hashed)
        val v2 = RSA("v2_" ++ axiom.hashed)
        // Predicates
        def atomA(t: Term): TupleTableAtom = {
          val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
          TupleTableAtom.create(graph, t, IRI.RDF_TYPE, cls)
        }
        def roleRf(t1: Term, t2: Term): TupleTableAtom =
          super.convert(roleR, t1, t2, Forward)
        def atomB(t: Term): TupleTableAtom = {
          val cls = axiom.getSuperClass
            .asInstanceOf[OWLObjectSomeValuesFrom]
            .getFiller
            .asInstanceOf[OWLClass]
            .getIRI
          TupleTableAtom.create(graph, t, IRI.RDF_TYPE, cls)
        }
        //Rules
        List(
          Rule.create(roleRf(v0, v1), atomA(v0)),
          Rule.create(atomB(v1), atomA(v0)),
          Rule.create(roleRf(v1, v2), atomA(v1)),
          Rule.create(atomB(v2), atomA(v1))
        )
      } else {
        List()
      }
    }

    private def rules3(axiom: OWLSubClassOfAxiom): List[Rule] = {
      val cycle = ontology.cycle(axiom).toList
      val roleR =
        axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getProperty
      // Fresh Variables
      val v1 = RSA("v1_" ++ axiom.hashed)
      // Predicates
      def atomA(t: Term): TupleTableAtom = {
        val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
        TupleTableAtom.create(graph, t, IRI.RDF_TYPE, cls)
      }
      def roleRf(t: Term): TupleTableAtom =
        super.convert(roleR, t, v1, Forward)
      val atomB: TupleTableAtom = {
        val cls = axiom.getSuperClass
          .asInstanceOf[OWLObjectSomeValuesFrom]
          .getFiller
          .asInstanceOf[OWLClass]
          .getIRI
        TupleTableAtom.create(graph, v1, IRI.RDF_TYPE, cls)
      }
      cycle.flatMap { x =>
        List(
          Rule.create(roleRf(x), atomA(x)),
          Rule.create(atomB, atomA(x))
        )
      }
    }

    override def convert(
        axiom: OWLLogicalAxiom,
        term: Term,
        unsafe: List[OWLObjectPropertyExpression],
        skolem: SkolemStrategy,
        suffix: RSASuffix
    )(implicit fresh: DataFactory): Result =
      axiom match {

        case a: OWLSubClassOfAxiom if a.isT5 => {
          val role = axiom.objectPropertyExpressionsInSignature(0)
          if (unsafe contains role)
            super.convert(a, term, unsafe, new Standard(a), Forward)(fresh)
          else {
            val (f1, r1) = rules1(a)
            (f1, r1 ::: rules2(a) ::: rules3(a))
          }
        }

        case a: OWLSubObjectPropertyOfAxiom => {
          val (facts, rules) = List(Empty, Forward, Backward)
            .map(super.convert(a, term, unsafe, NoSkolem, _)(fresh))
            .unzip
          (facts.flatten, rules.flatten)
        }

        case a => super.convert(a, term, unsafe, skolem, suffix)(fresh)

      }
  }

}
