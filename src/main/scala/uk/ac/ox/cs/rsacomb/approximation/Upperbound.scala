package uk.ac.ox.cs.rsacomb.approximation

// import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI => _, _}

import tech.oxfordsemantic.jrdfox.logic.expression.{Resource, IRI}

import scala.collection.mutable.Map
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.GraphTraversal._

import uk.ac.ox.cs.rsacomb.RSAOntology
import uk.ac.ox.cs.rsacomb.RSAUtil
import uk.ac.ox.cs.rsacomb.ontology.Ontology

object Upperbound {

  private val manager = OWLManager.createOWLOntologyManager()
  private val factory = manager.getOWLDataFactory()

}

/** Approximation algorithm that mantains completeness for CQ answering.
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
class Upperbound extends Approximation[RSAOntology] {

  /** Simplify conversion between Java and Scala collections */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Simplify conversion between OWLAPI and RDFox concepts */
  // import uk.ac.ox.cs.rsacomb.implicits.RDFox._

  /** Main entry point for the approximation algorithm */
  def approximate(ontology: Ontology): RSAOntology =
    toRSA(
      new Ontology(
        ontology.axioms flatMap toConjuncts,
        ontology.datafiles
      )
    )

  /** Discards all axioms outside ALCHOIQ */
  // private def inALCHOIQ(axiom: OWLLogicalAxiom): Boolean =
  //   axiom match {
  //     case a: OWLSubClassOfAxiom => {
  //       val sub = a.getSubClass.getNNF
  //       val sup = a.getSuperClass.getNNF
  //       (sub, sup) match {
  //         case (sub: OWLObjectAllValuesFrom, _) => false
  //         case (sub: OWLDataAllValuesFrom, _)   => false
  //         case (_, sup: OWLDataAllValuesFrom)   => false
  //         case (sub: OWLObjectMinCardinality, _) if sub.getCardinality >= 2 =>
  //           false
  //         case (sub: OWLDataMinCardinality, _) if sub.getCardinality >= 2 =>
  //           false
  //         case (_, sup: OWLObjectMinCardinality) if sup.getCardinality >= 2 =>
  //           false
  //         case (_, sup: OWLDataMinCardinality) if sup.getCardinality >= 2 =>
  //           false
  //         case (sub: OWLObjectMaxCardinality, _) => false
  //         case (sub: OWLDataMaxCardinality, _)   => false
  //         case (_, sup: OWLObjectMaxCardinality) if sup.getCardinality >= 2 =>
  //           false
  //         case (_, sup: OWLDataMaxCardinality) if sup.getCardinality >= 1 =>
  //           false
  //         case (_, sup: OWLObjectOneOf) if sup.getIndividuals.length > 2 =>
  //           false
  //         case (sub: OWLObjectHasSelf, _) => false
  //         case (_, sup: OWLObjectHasSelf) => false
  //         case _                          => true
  //       }
  //     }
  //     case a: OWLTransitiveObjectPropertyAxiom => false
  //     case a: OWLReflexiveObjectPropertyAxiom  => false
  //     case a: OWLSubPropertyChainOfAxiom       => false
  //     case a: OWLAsymmetricObjectPropertyAxiom => false
  //     case a                                   => true
  //   }

  /** Turn disjuncts into conjuncts
    *
    * This is a very naïve way of getting rid of disjunction preserving
    * completeness of CQ answering.
    *
    * @todo implement a choice function that decides which disjunct to
    * keep instead of keeping all of them. Note that PAGOdA is currently
    * doing something similar.
    */
  private def toConjuncts(axiom: OWLLogicalAxiom): List[OWLLogicalAxiom] =
    axiom match {
      case a: OWLSubClassOfAxiom => {
        val sub = a.getSubClass.getNNF
        val sup = a.getSuperClass.getNNF
        sup match {
          case sup: OWLObjectUnionOf =>
            sup.asDisjunctSet.map(
              Upperbound.factory.getOWLSubClassOfAxiom(sub, _)
            )
          case _ => List(axiom)
        }
      }
      case _ => List(axiom)
    }

  /** Approximate a Horn-ALCHOIQ ontology to RSA
    *
    * This is done by gathering those existential axioms that prevent
    * the ontology dependency graph from being tree-shaped and constant
    * skolemize them.
    *
    * @param ontology the set of axioms to approximate.
    * @return the approximated RSA ontology
    */
  private def toRSA(ontology: Ontology): RSAOntology = {
    /* Compute the dependency graph for the ontology */
    val (graph, nodemap) = ontology.dependencyGraph

    /* Define node colors for the graph visit */
    sealed trait NodeColor
    case object Unvisited extends NodeColor
    case object Visited extends NodeColor
    case object ToSkolem extends NodeColor

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
              color(source) = ToSkolem
            case ToSkolem =>
              color(source) = Visited
          }
        }
        case ToSkolem => {}
      }
    }

    val toSkolem = color.collect { case (resource: IRI, ToSkolem) =>
      nodemap(resource.getIRI)
    }.toList

    // Force constant skolemization by introducing a fresh individual
    // (singleton class).
    val skolemized = toSkolem flatMap { (axiom) =>
      import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._
      axiom.toTriple match {
        case Some((subclass, role, filler)) => {
          val skolem = Upperbound.factory.getOWLNamedIndividual(s"i_${axiom.toString.hashCode}")
          val fresh = RSAUtil.getFreshOWLClass()
          List(
            Upperbound.factory.getOWLSubClassOfAxiom(
              subclass,
              Upperbound.factory.getOWLObjectSomeValuesFrom(role, fresh)
            ),
            Upperbound.factory.getOWLSubClassOfAxiom(
              fresh,
              Upperbound.factory.getOWLObjectOneOf(skolem)
            ),
            Upperbound.factory.getOWLClassAssertionAxiom(filler, skolem)
          )
        }
        case None => List()
      }
    }

    /* Substitute selected axioms with their "skolemized" version */
    RSAOntology(
      ontology.axioms diff toSkolem concat skolemized,
      ontology.datafiles
    )
  }

  // val edges1 = Seq('A ~> 'B, 'B ~> 'C, 'C ~> 'D, 'D ~> 'H, 'H ~>
  // 'G, 'G ~> 'F, 'E ~> 'A, 'E ~> 'F, 'B ~> 'E, 'F ~> 'G, 'B ~> 'F,
  // 'C ~> 'G, 'D ~> 'C, 'H ~> 'D)
  // val edges2 = Seq('I ~> 'M, 'I ~> 'L, 'L ~> 'N, 'M ~> 'N)
  // val edges3 = Seq('P ~> 'O)
  // val graph = Graph.from(edges = edges1 ++ edges2 ++ edges3)
  
}
