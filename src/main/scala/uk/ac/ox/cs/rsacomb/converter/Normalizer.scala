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

import uk.ac.ox.cs.rsacomb.util.Logger

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

  private var counter = -1
  def freshOWLClass(): OWLClass = {
    counter += 1
    factory.getOWLClass(s"X$counter")
  }

  /** Statistics */
  var discarded = 0
  var shifted = 0

  /** Normalizes a
    * [[org.semanticweb.owlapi.model.OWLLogicalAxiom OWLLogicalAxiom]]
    *
    * @note not all possible axioms are supported. Following is a list
    * of all unhandled class expressions:
    * - [[org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom OWLAsymmetricObjectPropertyAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom OWLDatatypeDefinitionAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom OWLDisjointDataPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom OWLDisjointObjectPropertiesAxiom]]
    * - [[org.semanticweb.owlapi.model.OWLHasKeyAxiom OWLHasKeyAxiom]]
    * - [[org.semanticweb.owlapi.model.SWRLRule SWRLRule]]
    */
  def normalize(axiom: OWLLogicalAxiom): Seq[OWLLogicalAxiom] =
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
            val cls = freshOWLClass()
            Seq(
              factory.getOWLSubClassOfAxiom(sub, cls),
              factory.getOWLSubClassOfAxiom(cls, sup)
            ).flatMap(normalize)
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
                    val cls = freshOWLClass()
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
                .flatMap(normalize)
            } else {
              normalize(factory.getOWLSubClassOfAxiom(factory.getOWLThing, sup))
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
                .flatMap(normalize)
            } else {
              normalize(factory.getOWLSubClassOfAxiom(sub, factory.getOWLThing))
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
                .flatMap(normalize)
            } else {
              normalize(
                factory.getOWLSubClassOfAxiom(factory.getOWLNothing, sup)
              )
            }
          }
          /** Disjunction on the rhs is not supported directly
            *
            * Instead we `shift` the rule to eliminate the disjunction.
            */
          case (_, sup: OWLObjectUnionOf) =>
            shift(sub, sup) flatMap normalize
          /** Complex class expression on existential restriction on the lhs
            *
            *   exists R . C c D -> { C c X, exists R . X c D }
            */
          case (sub: OWLObjectSomeValuesFrom, _)
              if !sub.getFiller.isOWLClass => {
            val cls = freshOWLClass()
            Seq(
              factory.getOWLSubClassOfAxiom(sub.getFiller, cls),
              factory.getOWLSubClassOfAxiom(
                factory.getOWLObjectSomeValuesFrom(sub.getProperty, cls),
                sup
              )
            ).flatMap(normalize)
          }
          /** Complex class expression on existential restriction on the rhs
            *
            *   C c exists R . D -> { X c D, C c exists R . X }
            */
          case (_, sup: OWLObjectSomeValuesFrom)
              if !sup.getFiller.isOWLClass => {
            val cls = freshOWLClass()
            Seq(
              factory.getOWLSubClassOfAxiom(cls, sup.getFiller),
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectSomeValuesFrom(sup.getProperty, cls)
              )
            ).flatMap(normalize)
          }
          /** Object/Data universal quantification on the lhs not supported */
          case (sub: OWLObjectAllValuesFrom, _) => notInHornALCHOIQ(a)
          case (sub: OWLDataAllValuesFrom, _)   => notInHornALCHOIQ(a)
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
            )
          /** Object universal quantification on the rhs not supported */
          case (_, sup: OWLDataAllValuesFrom) => notInHornALCHOIQ(a)
          /** Exact object/data cardinality restriction on the lhs/rhs
            *
            *   = i R . C -> <= i R . C n >= i R . X
            */
          case (sub: OWLObjectExactCardinality, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub.asIntersectionOfMinMax, sup)
            )
          case (sub: OWLDataExactCardinality, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub.asIntersectionOfMinMax, sup)
            )
          case (_, sup: OWLObjectExactCardinality) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub, sup.asIntersectionOfMinMax)
            )
          case (_, sup: OWLDataExactCardinality) =>
            normalize(
              factory.getOWLSubClassOfAxiom(sub, sup.asIntersectionOfMinMax)
            )
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
                )
              case 1 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(
                    factory.getOWLObjectSomeValuesFrom(
                      sub.getProperty,
                      sub.getFiller
                    ),
                    sup
                  )
                )
              case _ => notInHornALCHOIQ(a)
            }
          case (sub: OWLDataMinCardinality, _) =>
            sub.getCardinality match {
              case 0 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(factory.getOWLThing, sup)
                )
              case 1 =>
                normalize(
                  factory.getOWLSubClassOfAxiom(
                    factory.getOWLDataSomeValuesFrom(
                      sub.getProperty,
                      sub.getFiller
                    ),
                    sup
                  )
                )
              case _ => notInHornALCHOIQ(a)
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
                )
              case _ => notInHornALCHOIQ(a)
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
                )
              case _ => notInHornALCHOIQ(a)
            }
          /** Max object/data cardinality restriction on the lhs not supported */
          case (sub: OWLObjectMaxCardinality, _) => notInHornALCHOIQ(a)
          case (sub: OWLDataMaxCardinality, _)   => notInHornALCHOIQ(a)
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
            )
          case (_, sup: OWLObjectMaxCardinality)
              if sup.getCardinality == 1 && !sup.getFiller.isOWLClass => {
            val cls = freshOWLClass()
            Seq(
              factory.getOWLSubClassOfAxiom(cls, sup.getFiller),
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectMaxCardinality(1, sup.getProperty, cls)
              )
            ).flatMap(normalize)
          }
          case (_, sup: OWLObjectMaxCardinality) if sup.getCardinality >= 2 =>
            notInHornALCHOIQ(a)
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
            )
          case (_, sup: OWLDataMaxCardinality) if sup.getCardinality >= 1 =>
            notInHornALCHOIQ(a)
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
            )
          case (sub: OWLDataHasValue, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLDataSomeValuesFrom(
                  sub.getProperty,
                  factory.getOWLDataOneOf(sub.getFiller)
                ),
                sup
              )
            )
          case (_, sup: OWLObjectHasValue) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLObjectSomeValuesFrom(
                  sup.getProperty,
                  factory.getOWLObjectOneOf(sup.getFiller)
                )
              )
            )
          case (_, sup: OWLDataHasValue) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                sub,
                factory.getOWLDataSomeValuesFrom(
                  sup.getProperty,
                  factory.getOWLDataOneOf(sup.getFiller)
                )
              )
            )
          /** Enumeration of individuals on the lhs
            *
            *   {a1, ... ,an} c D -> { D(a1), ..., D(an) }
            */
          case (sub: OWLObjectOneOf, _) =>
            sub.getIndividuals.map(factory.getOWLClassAssertionAxiom(sup, _))
          /** Enumeration of individuals on the rhs
            * It's supported only when of cardinality < 2.
            */
          case (_, sup: OWLObjectOneOf) if sup.getIndividuals.length == 0 =>
            normalize(factory.getOWLSubClassOfAxiom(sub, factory.getOWLNothing))
          case (_, sup: OWLObjectOneOf) if sup.getIndividuals.length > 2 =>
            notInHornALCHOIQ(a)
          /** Class complement on the lhs
            *
            *   ~C c D -> top c C n D
            */
          case (sub: OWLObjectComplementOf, _) =>
            normalize(
              factory.getOWLSubClassOfAxiom(
                factory.getOWLThing,
                factory.getOWLObjectIntersectionOf(sub.getComplementNNF, sup)
              )
            )
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
            )
          /** Self-restriction over an object property */
          case (sub: OWLObjectHasSelf, _) => notInHornALCHOIQ(a)
          case (_, sup: OWLObjectHasSelf) => notInHornALCHOIQ(a)

          /** Axiom is already normalized */
          case _ => Seq(a)
        }
      }

      case a: OWLEquivalentClassesAxiom => {
        a.getAxiomWithoutAnnotations.asOWLSubClassOfAxioms.flatMap(normalize)
      }

      case a: OWLEquivalentObjectPropertiesAxiom => {
        a.getAxiomWithoutAnnotations.asSubObjectPropertyOfAxioms.flatMap(
          normalize
        )
      }

      case a: OWLEquivalentDataPropertiesAxiom => {
        a.getAxiomWithoutAnnotations.asSubDataPropertyOfAxioms.flatMap(
          normalize
        )
      }

      case a: OWLObjectPropertyDomainAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLObjectPropertyRangeAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLDataPropertyDomainAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLDataPropertyRangeAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

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
          normalize
        )

      case a: OWLFunctionalObjectPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLFunctionalDataPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLInverseFunctionalObjectPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLSymmetricObjectPropertyAxiom =>
        a.getAxiomWithoutAnnotations.asSubPropertyAxioms.flatMap(normalize)

      case a: OWLDifferentIndividualsAxiom =>
        a.asPairwiseAxioms.map((a) => {
          val classes = a.getIndividuals.map(factory.getOWLObjectOneOf(_))
          factory.getOWLSubClassOfAxiom(
            factory.getOWLObjectIntersectionOf(classes),
            factory.getOWLNothing
          )
        })

      case a: OWLIrreflexiveObjectPropertyAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLSameIndividualAxiom =>
        a.getAxiomWithoutAnnotations.asOWLSubClassOfAxioms.flatMap(normalize)

      case a: OWLDisjointUnionAxiom =>
        Seq(a.getOWLDisjointClassesAxiom, a.getOWLEquivalentClassesAxiom)
          .flatMap(normalize)

      /** Complex class assertion
        *
        * C(a) -> { X(a), X c C }
        */
      case a: OWLClassAssertionAxiom if !a.getClassExpression.isOWLClass => {
        val cls = freshOWLClass()
        Seq(
          factory.getOWLClassAssertionAxiom(cls, a.getIndividual),
          factory.getOWLSubClassOfAxiom(cls, a.getClassExpression)
        ).flatMap(normalize)
      }

      case a: OWLNegativeObjectPropertyAssertionAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      case a: OWLNegativeDataPropertyAssertionAxiom =>
        normalize(a.getAxiomWithoutAnnotations.asOWLSubClassOfAxiom)

      /** Not in Horn-ALCHOIQ */

      case a: OWLTransitiveObjectPropertyAxiom => notInHornALCHOIQ(a)

      case a: OWLReflexiveObjectPropertyAxiom => notInHornALCHOIQ(a)

      case a: OWLSubPropertyChainOfAxiom => notInHornALCHOIQ(a)

      /** Unsupported */

      case a: OWLAsymmetricObjectPropertyAxiom => notInHornALCHOIQ(a)

      case a: OWLDatatypeDefinitionAxiom => notSupported(a)

      case a: OWLDisjointDataPropertiesAxiom => notSupported(a)

      case a: OWLDisjointObjectPropertiesAxiom => notSupported(a)

      case a: OWLHasKeyAxiom => notSupported(a)

      case a: SWRLRule => notSupported(a)

      /** Axiom is already normalized */
      case a => Seq(a)
    }

  /** Shift an axiom with disjunction on the rhs */
  private def shift(
      sub: OWLClassExpression,
      sup: OWLObjectUnionOf
  ): Seq[OWLLogicalAxiom] = {
    val body = sub.asConjunctSet.map((atom) => (atom, freshOWLClass()))
    val head = sup.asDisjunctSet.map((atom) => (atom, freshOWLClass()))

    /* Update statistics */
    shifted += 1

    val r1 =
      factory.getOWLSubClassOfAxiom(
        factory.getOWLObjectIntersectionOf(
          (body.map(_._1) ++ head.map(_._2)): _*
        ),
        factory.getOWLNothing
      )

    val r2s =
      for {
        (a, na) <- head
        hs = head.map(_._2).filterNot(_ equals na)
      } yield factory.getOWLSubClassOfAxiom(
        factory.getOWLObjectIntersectionOf(
          (body.map(_._1) ++ hs): _*
        ),
        a
      )

    val r3s =
      for {
        (a, na) <- body
        bs = body.map(_._1).filterNot(_ equals a)
      } yield factory.getOWLSubClassOfAxiom(
        factory.getOWLObjectIntersectionOf(
          (bs ++ head.map(_._2)): _*
        ),
        na
      )

    Seq(r1) ++ r2s ++ r3s
  }

  /** Approximation function for axioms out of Horn-ALCHOIQ
    *
    * By default discards the axiom, which guarantees a lower bound
    * ontology w.r.t. CQ answering.
    */
  protected def notInHornALCHOIQ(
      axiom: OWLLogicalAxiom
  ): Seq[OWLLogicalAxiom] = {
    /* Update statistics */
    discarded += 1
    Logger.print(
      s"'$axiom' has been ignored because it is not in Horn-ALCHOIQ",
      Logger.VERBOSE
    )
    Seq()
  }

  /** Non supported axioms */
  private def notSupported(axiom: OWLLogicalAxiom): Seq[OWLLogicalAxiom] =
    throw new RuntimeException(
      s"'$axiom' is not currently supported."
    )
}
