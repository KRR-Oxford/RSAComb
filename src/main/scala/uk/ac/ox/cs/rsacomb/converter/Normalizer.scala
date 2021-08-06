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

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import uk.ac.ox.cs.rsacomb.util.{Logger, DataFactory}
import uk.ac.ox.cs.rsacomb.RSAOntology

object Normalizer {

  /** Factory used for converting expressions when needed.
    *
    * @note most of the time this is used to perform some kind of
    * normalization on axioms. In later versions it might be worth
    * moving the normalization code in its own independent step.
    */
  private val manager = OWLManager.createOWLOntologyManager()
  val factory = manager.getOWLDataFactory()

}

class Normalizer() {

  import Normalizer._

  /** Simplify conversion between Java and Scala collections */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Normalizes a [[OWLLogicalAxiom]]
    */
  def normalize(
      axiom: OWLLogicalAxiom
  )(implicit fresh: DataFactory): Seq[OWLLogicalAxiom] =
    axiom match {
      case a: OWLSubClassOfAxiom => {
        val sub = a.getSubClass.getNNF
        val sup = a.getSuperClass.getNNF
        (sub, sup) match {
          /** Split complex subclass axioms
            *
            *   C c D -> { C c X, X c D }
            */
          case _ if !sub.isOWLClass && !sup.isOWLClass => {
            val cls = fresh.getOWLClass
            Seq(
              factory.getOWLSubClassOfAxiom(sub, cls),
              factory.getOWLSubClassOfAxiom(cls, sup)
            ).flatMap(normalize(_)(fresh))
          }
          /** Conjunction on the lhs
            *
            *   A1 n ... n C n ... n An c D -> { C c X, A1 n ... n X n ... n An c D }
            */
          case (sub: OWLObjectIntersectionOf, _)
              if sub.asConjunctSet.exists(c => !c.isOWLClass) => {
            var additional = Seq()
            val conjuncts = sub.asConjunctSet
            if (conjuncts.length > 0) {
              val acc = (Seq[OWLClassExpression](), Seq[OWLLogicalAxiom]())
              val (acc1, acc2) = conjuncts.foldLeft(acc)(
                { case ((acc1, acc2), conj) =>
                  if (conj.isOWLClass)
                    (acc1 :+ conj, acc2)
                  else {
                    val cls = fresh.getOWLClass
                    (
                      acc1 :+ cls,
                      acc2 :+ factory.getOWLSubClassOfAxiom(conj, cls)
                    )
                  }
                }
              )
              (acc2 :+ factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectIntersectionOf(acc1: _*),
                sup
              ))
                .flatMap(normalize(_)(fresh))
            } else {
              normalize(
                factory.getOWLSubClassOfAxiom(factory.getOWLThing, sup)
              )(fresh)
            }
          }
          /** Conjunction on the rhs
            *
            *   D c A1 n ... n An -> { D c A1, ... , D c An }
            */
          case (_, sup: OWLObjectIntersectionOf) => {
            val conjuncts = sup.asConjunctSet
            if (conjuncts.length > 0) {
              conjuncts
                .map(cls => factory.getOWLSubClassOfAxiom(sub, cls))
                .flatMap(normalize(_)(fresh))
            } else {
              normalize(
                factory.getOWLSubClassOfAxiom(sub, factory.getOWLThing)
              )(fresh)
            }
          }
          /** Disjunction on the lhs
            *
            *   A1 u ... u An c D -> { A1 c D, ... , An c D }
            */
          case (sub: OWLObjectUnionOf, _) => {
            val disjuncts = sub.asDisjunctSet
            if (disjuncts.length > 0) {
              disjuncts
                .map(cls => factory.getOWLSubClassOfAxiom(cls, sup))
                .flatMap(normalize(_)(fresh))
            } else {
              normalize(
                factory.getOWLSubClassOfAxiom(factory.getOWLNothing, sup)
              )(fresh)
            }
          }
          /** Disjunction on the rhs
            *
            *   B c A1 u ... u C u ... u An -> { X c C, B c A1 u ... u X u ... u An }
            */
          case (_, sup: OWLObjectUnionOf)
              if sup.asDisjunctSet.exists(c => !c.isOWLClass) => {
            var additional = Seq()
            val disjuncts = sup.asDisjunctSet
            // BUG: why test for legth if this branch gets triggered only
            // when there exists a ClassExpression in the disjuncts?
            if (disjuncts.length > 0) {
              val acc = (Seq[OWLClassExpression](), Seq[OWLLogicalAxiom]())
              val (acc1, acc2) = disjuncts.foldLeft(acc)(
                {
                  case ((acc1, acc2), disj: OWLClass) => (acc1 :+ disj, acc2)
                  case ((acc1, acc2), disj) => {
                    val cls = fresh.getOWLClass
                    (
                      acc1 :+ cls,
                      acc2 :+ factory.getOWLSubClassOfAxiom(cls, disj)
                    )
                  }
                }
              )
              (acc2 :+ factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectUnionOf(acc1: _*)
              )).flatMap(normalize(_)(fresh))
            } else {
              normalize(
                factory.getOWLSubClassOfAxiom(sub, factory.getOWLNothing)
              )(fresh)
            }
          }
          /** Complex class expression on existential restriction on the lhs
            *
            *   exists R . C c D -> { C c X, exists R . X c D }
            */
          case (sub: OWLObjectSomeValuesFrom, _)
              if !sub.getFiller.isOWLClass => {
            val cls = fresh.getOWLClass
            Seq(
              factory.getOWLSubClassOfAxiom(sub.getFiller, cls),
              factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectSomeValuesFrom(sub.getProperty, cls),
                sup
              )
            ).flatMap(normalize(_)(fresh))
          }
          /** Complex class expression on existential restriction on the rhs
            *
            *   C c exists R . D -> { X c D, C c exists R . X }
            */
          case (_, sup: OWLObjectSomeValuesFrom)
              if !sup.getFiller.isOWLClass => {
            val cls = fresh.getOWLClass
            Seq(
              factory.getOWLSubClassOfAxiom(cls, sup.getFiller),
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectSomeValuesFrom(sup.getProperty, cls)
              )
            ).flatMap(normalize(_)(fresh))
          }
          /** Object universal quantification on the lhs
            *
            *   forall R . B c A
            *   ¬ A c ¬∀forall R . B
            *   ¬ A c exists R . ¬ B
            *   ¬ A c C, C c R . ¬ B
            *   top c A u C, D c ¬ B, C c exists R . D
            *   top c A u C, D n B c bot, C c exists R . D
            */
          case (sub: OWLObjectAllValuesFrom, _) => {
            val role = sub.getProperty
            val filler = sub.getFiller
            val (c, d) = (fresh.getOWLClass, fresh.getOWLClass)
            Seq(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLThing,
                factory.getOWLObjectUnionOf(sup, c)
              ),
              factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectIntersectionOf(d, filler),
                factory.getOWLNothing
              ),
              factory.getOWLSubClassOfAxiom(
                c,
                factory.getOWLObjectSomeValuesFrom(role, d)
              )
            )
          }
          /** Object/Data universal quantification on the lhs */
          case (sub: OWLDataAllValuesFrom, _) => notSupported(a)
          /** Object universal quantification on the rhs
            *
            *   C c forall R . D -> exists R- . C c D
            */
          case (_, sup: OWLObjectAllValuesFrom) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory
                  .getOWLObjectSomeValuesFrom(
                    sup.getProperty.getInverseProperty,
                    sub
                  ),
                sup.getFiller
              )
            )(fresh)
          /** Object universal quantification on the rhs not supported */
          case (_, sup: OWLDataAllValuesFrom) => notSupported(a)
          /** Exact object/data cardinality restriction on the lhs/rhs
            *
            *   = i R . C -> <= i R . C n >= i R . X
            */
          case (sub: OWLObjectExactCardinality, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub.asIntersectionOfMinMax, sup)
            )(fresh)
          case (sub: OWLDataExactCardinality, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub.asIntersectionOfMinMax, sup)
            )(fresh)
          case (_, sup: OWLObjectExactCardinality) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub, sup.asIntersectionOfMinMax)
            )(fresh)
          case (_, sup: OWLDataExactCardinality) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub, sup.asIntersectionOfMinMax)
            )(fresh)
          /** Min object/data cardinality restriction on the lhs/rhs
            *
            *   >= 0 R . C -> top
            *   >= 1 R . C -> exists R . C
            *
            * @note not supported when restriction >= 2.
            */
          case (sub: OWLObjectMinCardinality, _) =>
            sub.getCardinality match {
              case 0 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(factory.getOWLThing, sup)
                )(fresh)
              case 1 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(
                    factory.getOWLObjectSomeValuesFrom(
                      sub.getProperty,
                      sub.getFiller
                    ),
                    sup
                  )
                )(fresh)
              case _ => notSupported(a)
            }
          case (sub: OWLDataMinCardinality, _) =>
            sub.getCardinality match {
              case 0 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(factory.getOWLThing, sup)
                )(fresh)
              case 1 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(
                    factory.getOWLDataSomeValuesFrom(
                      sub.getProperty,
                      sub.getFiller
                    ),
                    sup
                  )
                )(fresh)
              case _ => notSupported(a)
            }
          case (_, sup: OWLObjectMinCardinality) =>
            sup.getCardinality match {
              case 0 => Seq()
              case 1 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(
                    sub,
                    factory.getOWLObjectSomeValuesFrom(
                      sup.getProperty,
                      sup.getFiller
                    )
                  )
                )(fresh)
              case _ => notSupported(a)
            }
          case (_, sup: OWLDataMinCardinality) =>
            sup.getCardinality match {
              case 0 => Seq()
              case 1 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(
                    sub,
                    factory.getOWLDataSomeValuesFrom(
                      sup.getProperty,
                      sup.getFiller
                    )
                  )
                )(fresh)
              case _ => notSupported(a)
            }
          /** Max object/data cardinality restriction on the lhs not supported */
          case (sub: OWLObjectMaxCardinality, _) => notSupported(a)
          case (sub: OWLDataMaxCardinality, _)   => notSupported(a)
          /** Max object/data cardinality restriction on the rhs
            *
            *   C c <= 0 R . D -> C n exists R . D -> bot
            *   C c <= 1 R . D -> { X c D, C c <= 1 R . D }
            *
            * @note not supported when restriction >= 2.
            */
          case (_, sup: OWLObjectMaxCardinality) if sup.getCardinality == 0 =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectIntersectionOf(
                  sub,
                  factory
                    .getOWLObjectSomeValuesFrom(sup.getProperty, sup.getFiller)
                ),
                factory.getOWLNothing
              )
            )(fresh)
          case (_, sup: OWLObjectMaxCardinality)
              if sup.getCardinality == 1 && !sup.getFiller.isOWLClass => {
            val cls = fresh.getOWLClass
            Seq(
              factory.getOWLSubClassOfAxiom(cls, sup.getFiller),
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectMaxCardinality(1, sup.getProperty, cls)
              )
            ).flatMap(normalize(_)(fresh))
          }
          case (_, sup: OWLObjectMaxCardinality) if sup.getCardinality >= 2 =>
            notSupported(a)
          case (_, sup: OWLDataMaxCardinality) if sup.getCardinality == 0 =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectIntersectionOf(
                  sub,
                  factory
                    .getOWLDataSomeValuesFrom(sup.getProperty, sup.getFiller)
                ),
                factory.getOWLNothing
              )
            )(fresh)
          case (_, sup: OWLDataMaxCardinality) if sup.getCardinality >= 1 =>
            notSupported(a)
          /** HasValue expression on the lhs/rhs
            *
            * HasValue(R, a) -> exists R . {a}
            */
          case (sub: OWLObjectHasValue, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectSomeValuesFrom(
                  sub.getProperty,
                  factory.getOWLObjectOneOf(sub.getFiller)
                ),
                sup
              )
            )(fresh)
          case (sub: OWLDataHasValue, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLDataSomeValuesFrom(
                  sub.getProperty,
                  factory.getOWLDataOneOf(sub.getFiller)
                ),
                sup
              )
            )(fresh)
          case (_, sup: OWLObjectHasValue) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectSomeValuesFrom(
                  sup.getProperty,
                  factory.getOWLObjectOneOf(sup.getFiller)
                )
              )
            )(fresh)
          case (_, sup: OWLDataHasValue) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLDataSomeValuesFrom(
                  sup.getProperty,
                  factory.getOWLDataOneOf(sup.getFiller)
                )
              )
            )(fresh)
          /** Enumeration of individuals on the lhs
            *
            *   {a1, ... ,an} c D -> { D(a1), ..., D(an) }
            */
          case (sub: OWLObjectOneOf, _) =>
            sub.getIndividuals.map(factory.getOWLClassAssertionAxiom(sup, _))
          /** Enumeration of individuals on the rhs
            *
            *   A c {a1, ... ,an} -> { A c {a1} u ... u {an} }
            */
          case (_, sup: OWLObjectOneOf) if sup.getIndividuals.length > 2 =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectUnionOf(
                  sup.getIndividuals.map(factory.getOWLObjectOneOf(_))
                )
              )
            )(fresh)
          /** Class complement on the lhs
            *
            *   ~C c D -> top c C u D
            */
          case (sub: OWLObjectComplementOf, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLThing,
                factory.getOWLObjectUnionOf(sub.getComplementNNF, sup)
              )
            )(fresh)
          /** Class complement on the rhs
            *
            *   C c ~D -> C n D c bot
            */
          case (_, sup: OWLObjectComplementOf) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectIntersectionOf(sup.getComplementNNF, sub),
                factory.getOWLNothing
              )
            )(fresh)
          /** Self-restriction over an object property */
          case (sub: OWLObjectHasSelf, _) => notSupported(a)
          case (_, sup: OWLObjectHasSelf) => notSupported(a)

          /** Axiom is already normalized */
          case _ => Seq(a)
        }
      }

      case a: OWLEquivalentClassesAxiom => {
        a.getAxiomWithoutAnnotations.asOWLSubClassOfAxioms.flatMap(
          normalize(_)(fresh)
        )
      }

      case a: OWLEquivalentObjectPropertiesAxiom => {
        a.getAxiomWithoutAnnotations.asSubObjectPropertyOfAxioms.flatMap(
          normalize(_)(fresh)
        )
      }

      case a: OWLEquivalentDataPropertiesAxiom => {
        a.getAxiomWithoutAnnotations.asSubDataPropertyOfAxioms.flatMap(
          normalize(_)(fresh)
        )
      }

      case a: OWLObjectPropertyDomainAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLObjectPropertyRangeAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLDataPropertyDomainAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLDataPropertyRangeAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLDisjointClassesAxiom =>
        a.asPairwiseAxioms.map((a) => {
          val classes = a.getAxiomWithoutAnnotations.getClassExpressions
          factory.getOWLSubClassOfAxiom(
            factory.getOWLObjectIntersectionOf(classes),
            factory.getOWLNothing
          )
        })

      case a: OWLInverseObjectPropertiesAxiom =>
        a.getAxiomWithoutAnnotations.asSubObjectPropertyOfAxioms.flatMap(
          normalize(_)(fresh)
        )

      case a: OWLFunctionalObjectPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLFunctionalDataPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLInverseFunctionalObjectPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLSymmetricObjectPropertyAxiom =>
        a.getAxiomWithoutAnnotations.asSubPropertyAxioms.flatMap(
          normalize(_)(fresh)
        )

      case a: OWLDifferentIndividualsAxiom =>
        a.asPairwiseAxioms.map((a) => {
          val classes = a.getIndividuals.map(factory.getOWLObjectOneOf(_))
          factory.getOWLSubClassOfAxiom(
            factory.getOWLObjectIntersectionOf(classes),
            factory.getOWLNothing
          )
        })

      case a: OWLIrreflexiveObjectPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLSameIndividualAxiom =>
        a.getAxiomWithoutAnnotations.asOWLSubClassOfAxioms.flatMap(
          normalize(_)(fresh)
        )

      case a: OWLDisjointUnionAxiom =>
        Seq(a.getOWLDisjointClassesAxiom, a.getOWLEquivalentClassesAxiom)
          .flatMap(normalize(_)(fresh))

      /** Complex class assertion
        *
        * C(a) -> { X(a), X c C }
        */
      case a: OWLClassAssertionAxiom if !a.getClassExpression.isOWLClass => {
        val cls = fresh.getOWLClass
        Seq(
          factory.getOWLClassAssertionAxiom(cls, a.getIndividual),
          factory.getOWLSubClassOfAxiom(cls, a.getClassExpression)
        ).flatMap(normalize(_)(fresh))
      }

      case a: OWLNegativeObjectPropertyAssertionAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLNegativeDataPropertyAssertionAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLTransitiveObjectPropertyAxiom => {
        val role = a.getProperty
        normalize(
          factory.getOWLSubPropertyChainOfAxiom(List(role, role), role)
        )(fresh)
      }

      case a: OWLReflexiveObjectPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)(fresh)

      case a: OWLAsymmetricObjectPropertyAxiom => notSupported(a)

      case a: OWLDatatypeDefinitionAxiom => notSupported(a)

      case a: OWLDisjointDataPropertiesAxiom => notSupported(a)

      case a: OWLDisjointObjectPropertiesAxiom => notSupported(a)

      case a: OWLHasKeyAxiom => notSupported(a)

      case a: SWRLRule => notSupported(a)

      /** Axiom is already normalized */
      //case a: OWLSubPropertyChainOfAxiom => notSupported(a)
      case a => Seq(a)
    }

  /** Non supported axioms */
  private def notSupported(axiom: OWLLogicalAxiom): Seq[OWLLogicalAxiom] =
    // Logger.print(
    //   s"'$axiom' has been ignored because it is not in Horn-ALCHOIQ",
    //   Logger.VERBOSE
    // )
    // Seq()
    throw new RuntimeException(
      s"'$axiom' is not currently supported."
    )
}
