package uk.ac.ox.cs.rsacomb.approximation

import java.io.File

import org.semanticweb.owlapi.model._

import scala.collection.mutable.{Set, Map}
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.GraphTraversal._

import uk.ac.ox.cs.rsacomb.converter.Normalizer

/** Approximation algorithm that mantains soundness for CQ answering.
  *
  * The input OWL 2 ontology is assumed to be normalized and the output
  * ontology is guaranteed to be in RSA.
  *
  * The algorithm is performed in three steps:
  * 1. the ontology is reduced to ALCHOIQ by discarding any axiom
  *    that is not in the language;
  * 2. the ontology is further reduced to Horn-ALCHOIQ by shifting
  *    axioms with disjunction on the rhs;
  * 3. the ontology is approximated to RSA by manipulating its
  *    dependency graph.
  *
  * @see [[uk.ac.ox.cs.rsacomb.converter.Normalizer]]
  */
class LowerBound extends Approximation {

  val normalizer = new Normalizer()

  /** Main entry point for the approximation algorithm */
  def approximate(
      ontology: List[OWLLogicalAxiom],
      datafiles: List[File]
  ): List[OWLLogicalAxiom] = {
    /* Normalize axioms */
    val axioms1 = axioms flatMap normalizer.normalize(_)
    /* Delete any axiom outside of ALCHOIQ */
    val axioms2 = axioms1 filterNot inHornLACHOIQ
    /* Shift any axiom with disjunction on the rhs */
    val axioms3 = for {
      a1 <- axioms1
      a2 <- shift(a1)
      a3 <- normalize(a2)
    } yield a3
    /* Approximate to RSA */
    toRSA(axioms3, datafiles)
  }

  /** Discards all axioms outside ALCHOIQ */
  def inALCHOIQ(axiom: OWLLogicalAxiom): Boolean =
    axiom match {
      case a: OWLSubClassOfAxiom => {
        val sub = a.getSubClass.getNNF
        val sup = a.getSuperClass.getNNF
        (sub, sup) match {
          case (sub: OWLObjectAllValuesFrom, _) => false
          case (sub: OWLDataAllValuesFrom, _)   => false
          case (_, sup: OWLDataAllValuesFrom)   => false
          case (sub: OWLObjectMinCardinality, _) if sub.getCardinality >= 2 =>
            false
          case (sub: OWLDataMinCardinality, _) if sub.getCardinality >= 2 =>
            false
          case (_, sup: OWLObjectMinCardinality) if sup.getCardinality >= 2 =>
            false
          case (_, sup: OWLDataMinCardinality) if sup.getCardinality >= 2 =>
            false
          case (sub: OWLObjectMaxCardinality, _) => false
          case (sub: OWLDataMaxCardinality, _)   => false
          case (_, sup: OWLObjectMaxCardinality) if sup.getCardinality >= 2 =>
            false
          case (_, sup: OWLDataMaxCardinality) if sup.getCardinality >= 1 =>
            false
          case (_, sup: OWLObjectOneOf) if sup.getIndividuals.length > 2 =>
            false
          case (sub: OWLObjectHasSelf, _) => false
          case (_, sup: OWLObjectHasSelf) => false
          case _                          => true
        }
      }
      case a: OWLTransitiveObjectPropertyAxiom => false
      case a: OWLReflexiveObjectPropertyAxiom  => false
      case a: OWLSubPropertyChainOfAxiom       => false
      case a: OWLAsymmetricObjectPropertyAxiom => false
      case a                                   => true
    }

  /** Shifting axioms with disjunction on the rhs.
    *
    * The process of shifting presenves soundness but completenes w.r.t.
    * CQ answering is lost.
    *
    * @example
    *
    *   A -> B1 u B2 u B3 .
    *
    *   becomes
    *
    *   A n nB1 n nB2 -> B3 .
    *   A n nB1 n nB3 -> B2 .
    *   A n nB2 n nB3 -> B1 .
    *   nB1 n nB2 n nB3 -> nA .
    *
    *   where nA, nB1, nB2, nB3 are fresh predicates "corresponding" to
    *   the negation of A, B1, B2, B3 respectively.
    */
  def shift(axiom: OWLLogicalAxiom): List[OWLLogicalAxiom] =
    axiom match {
      case a: OWLSubClassOfAxiom => {
        val sub = a.getSubClass.getNNF
        val sup = a.getSuperClass.getNNF
        sup match {
          case sup: OWLObjectUnionOf => {
            val body = sub.asConjunctSet.map((atom) => (atom, freshOWLClass()))
            val head = sup.asDisjunctSet.map((atom) => (atom, freshOWLClass()))

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

            List(r1) ++ r2s ++ r3s
          }
          case _ => List(axiom)
        }
      }
      case _ => List(axiom)
    }

  /** Approximate a Horn-ALCHOIQ ontology to RSA
    *
    * This is done by gathering those axioms that prevent the ontology
    * dependency graph from being tree-shaped, and removing them.
    *
    * @param axioms the set of axioms to approximate.
    * @return the approximated set of axioms.
    */
  def toRSA(
      axioms: List[OWLLogicalAxiom],
      datafiles: List[File]
  ): List[OWLLogicalAxiom] = {
    /* Compute the dependency graph for the ontology */
    val (graph, nodemap) = RSAUtil.dependencyGraph(axioms, datafiles)

    /* Define node colors for the graph visit */
    sealed trait NodeColor
    case object Unvisited extends NodeColor
    case object Visited extends NodeColor
    case object ToDelete extends NodeColor

    /* Keep track of node colors during graph visit */
    var color = Map.from[Resource, NodeColor](
      graph.nodes.toOuter.map(k => (k, Unvisited))
    )

    for {
      component <- graph.componentTraverser().map(_ to Graph)
      edge <- component
        .outerEdgeTraverser(component.nodes.head)
        .withKind(BreadthFirst)
    } yield {
      val source = edge._1
      val target = edge._2
      color(source) match {
        case Unvisited | Visited => {
          color(target) match {
            case Unvisited =>
              color(source) = Visited;
              color(target) = Visited
            case Visited =>
              color(source) = ToDelete
            case ToDelete =>
              color(source) = Visited
          }
        }
        case ToDelete =>
      }
    }

    val toDelete = color.iterator.collect { case (resource: IRI, ToDelete) =>
      nodemap(resource.getIRI)
    }.toList

    /* Remove axioms from approximated ontology */
    axioms diff toDelete
  }

  // val edges1 = Seq('A ~> 'B, 'B ~> 'C, 'C ~> 'D, 'D ~> 'H, 'H ~>
  // 'G, 'G ~> 'F, 'E ~> 'A, 'E ~> 'F, 'B ~> 'E, 'F ~> 'G, 'B ~> 'F,
  // 'C ~> 'G, 'D ~> 'C, 'H ~> 'D)
  // val edges2 = Seq('I ~> 'M, 'I ~> 'L, 'L ~> 'N, 'M ~> 'N)
  // val edges3 = Seq('P ~> 'O)
  // val graph = Graph.from(edges = edges1 ++ edges2 ++ edges3)
  //
}
