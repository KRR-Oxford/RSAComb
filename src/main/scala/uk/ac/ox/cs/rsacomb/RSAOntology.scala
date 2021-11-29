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

/* Java imports */
import java.{util => ju}
import java.util.HashMap
import java.util.stream.{Collectors, Stream}
import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.util.OWLOntologyMerger
import org.semanticweb.owlapi.model.{OWLOntology, OWLAxiom, OWLLogicalAxiom}
import org.semanticweb.owlapi.model.{
  OWLClass,
  OWLClassExpression,
  OWLDataProperty,
  OWLDataPropertyAssertionAxiom,
  OWLObjectProperty,
  OWLSubObjectPropertyOfAxiom,
  OWLObjectPropertyExpression,
  OWLObjectSomeValuesFrom,
  OWLDataSomeValuesFrom,
  OWLSubClassOfAxiom
}
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl

import tech.oxfordsemantic.jrdfox.client.{
  DataStoreConnection,
  TransactionType,
  UpdateType
}
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  BodyFormula,
  FilterAtom,
  Negation,
  Rule,
  TupleTableAtom,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  FunctionCall,
  IRI,
  Literal,
  Resource,
  Term,
  Variable
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

/* Scala imports */
import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._
import scala.collection.mutable.{Set, Map}
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/* Debug only */
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer
import tech.oxfordsemantic.jrdfox.logic._
import org.semanticweb.owlapi.model.OWLObjectInverseOf

import uk.ac.ox.cs.rsacomb.approximation.Approximation
import uk.ac.ox.cs.rsacomb.converter._
import uk.ac.ox.cs.rsacomb.filtering.{FilteringProgram, FilterType}
import uk.ac.ox.cs.rsacomb.suffix._
import uk.ac.ox.cs.rsacomb.sparql._
import uk.ac.ox.cs.rsacomb.util.{RDFoxUtil, RSA}
import uk.ac.ox.cs.rsacomb.util.Logger
import uk.ac.ox.cs.rsacomb.ontology.Ontology

object RSAOntology {

  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Manager instance to interface with OWLAPI */
  val manager = OWLManager.createOWLOntologyManager()

  /** Name of the RDFox data store used for CQ answering */
  private val DataStore = "answer_computation"

  /** Canonical model named graph */
  private val CanonGraph: IRI =
    RDFoxUtil.getNamedGraph(DataStore, "CanonicalModel")

  /** Filtering program named graph
    *
    * @param query query associated with the returned named graph.
    *
    * @return named graph for the filtering program associated with the
    * input query.
    */
  private def FilterGraph(query: ConjunctiveQuery): IRI =
    RDFoxUtil.getNamedGraph(DataStore, s"Filter${query.id}")

  /** Filtering program for a given query
    *
    * @param query the query to derive the filtering program
    * @return the filtering program for the given query
    */
  def filteringProgram(query: ConjunctiveQuery): FilteringProgram =
    Logger.timed(
      {
        val filter = FilteringProgram(FilterType.REVISEDv2)
        filter(CanonGraph, FilterGraph(query), query)
      },
      "Generating filtering program",
      Logger.DEBUG
    )

  def apply(
      origin: OWLOntology,
      axioms: List[OWLLogicalAxiom],
      datafiles: List[os.Path]
  ): RSAOntology = new RSAOntology(origin, axioms, datafiles)

}

/** A wrapper for an RSA ontology
  *
  * @param ontology the input OWL2 ontology.
  * @param datafiles additinal data (treated as part of the ABox)
  */
class RSAOntology(
    origin: OWLOntology,
    axioms: List[OWLLogicalAxiom],
    datafiles: List[os.Path]
) extends Ontology(origin, axioms, datafiles) {

  /** Simplify conversion between OWLAPI and RDFox concepts */
  import implicits.RDFox._
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._

  /** Simplify conversion between Java and Scala collections */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Set of axioms removed during the approximation to RSA */
  //private var removed: Seq[OWLAxiom] = Seq.empty

  /** Retrieve individuals/literals in the ontology */
  private val individuals: List[IRI] =
    ontology
      .getIndividualsInSignature(Imports.INCLUDED)
      .asScala
      .map(_.getIRI)
      .map(implicits.RDFox.owlapiToRdfoxIri)
      .toList
  private val literals: List[Literal] =
    axioms
      .collect { case a: OWLDataPropertyAssertionAxiom => a }
      .map(_.getObject)
      .map(implicits.RDFox.owlapiToRdfoxLiteral)

  /** Retrieve concepts/roles in the ontology */
  val concepts: List[OWLClass] =
    ontology.getClassesInSignature().asScala.toList
  val objroles: List[OWLObjectPropertyExpression] =
    axioms.flatMap(_.objectPropertyExpressionsInSignature).distinct
  val dataroles: List[OWLDataProperty] = origin.getDataPropertiesInSignature

  /** Unsafe roles of a given ontology.
    *
    * Unsafety conditions are the following:
    *
    * 1) For all roles r1 appearing in an axiom of type T5, r1 is unsafe
    *    if there exists a role r2 (different from top) appearing in an
    *    axiom of type T3 and r1 is a subproperty of the inverse of r2.
    *
    * 2) For all roles p1 appearing in an axiom of type T5, p1 is unsafe
    *    if there exists a role p2 appearing in an axiom of type T4 and
    *    p1 is a subproperty of either p2 or the inverse of p2.
    */
  // val unsafeRoles: List[OWLObjectPropertyExpression] = {

  //   /* Checking for unsafety condition (1) */
  //   val unsafe1 = for {
  //     axiom <- axioms
  //     if axiom.isT5
  //     role1 <- axiom.objectPropertyExpressionsInSignature
  //     roleSuper = role1 +: reasoner.superObjectProperties(role1)
  //     roleSuperInv = roleSuper.map(_.getInverseProperty)
  //     axiom <- axioms
  //     if axiom.isT3 && !axiom.isT3top
  //     role2 <- axiom.objectPropertyExpressionsInSignature
  //     if roleSuperInv contains role2
  //   } yield role1

  //   /* Checking for unsafety condition (2) */
  //   val unsafe2 = for {
  //     axiom <- axioms
  //     if axiom.isT5
  //     role1 <- axiom.objectPropertyExpressionsInSignature
  //     roleSuper = role1 +: reasoner.superObjectProperties(role1)
  //     roleSuperInv = roleSuper.map(_.getInverseProperty)
  //     axiom <- axioms
  //     if axiom.isT4
  //     role2 <- axiom.objectPropertyExpressionsInSignature
  //     if roleSuper.contains(role2) || roleSuperInv.contains(role2)
  //   } yield role1

  //   unsafe1 ++ unsafe2
  // }

  /** Approximate a Horn-ALCHOIQ ontology to RSA
    *
    * This is done by gathering those axioms that prevent the ontology
    * dependency graph `dependencyGraph` from being tree-shaped, and
    * removing them.
    *
    * @param graph the graph used to compute the axioms to remove.
    * @param nodemap map from graph nodes to ontology axioms.
    */
  // def toRSA(): RSAOntology = Logger.timed(
  //   {

  //     /* Compute the dependency graph for the ontology */
  //     val (graph, nodemap) = this.dependencyGraph()

  //     /* Define node colors for the graph visit */
  //     sealed trait NodeColor
  //     case object Unvisited extends NodeColor
  //     case object Visited extends NodeColor
  //     case object ToDelete extends NodeColor

  //     /* Keep track of node colors during graph visit */
  //     var color = Map.from[Resource, NodeColor](
  //       graph.nodes.toOuter.map(k => (k, Unvisited))
  //     )

  //     for {
  //       component <- graph.componentTraverser().map(_ to Graph)
  //       edge <- component
  //         .outerEdgeTraverser(component.nodes.head)
  //         .withKind(BreadthFirst)
  //     } yield {
  //       val source = edge._1
  //       val target = edge._2
  //       color(source) match {
  //         case Unvisited | Visited => {
  //           color(target) match {
  //             case Unvisited =>
  //               color(source) = Visited;
  //               color(target) = Visited
  //             case Visited =>
  //               color(source) = ToDelete
  //             case ToDelete =>
  //               color(source) = Visited
  //           }
  //         }
  //         case ToDelete =>
  //       }
  //     }

  //     val toDelete = color.iterator.collect { case (resource: IRI, ToDelete) =>
  //       nodemap(resource.getIRI)
  //     }.toSeq

  //     /* Remove axioms from approximated ontology */
  //     ontology.removeAxioms(toDelete: _*)
  //     this.removed = toDelete

  //     /* Return RSA ontology */
  //     RSAOntology(ontology, datafiles: _*)
  //   },
  //   "Horn-ALCHOIQ to RSA approximation:",
  //   Logger.DEBUG
  // )
  // val edges1 = Seq('A ~> 'B, 'B ~> 'C, 'C ~> 'D, 'D ~> 'H, 'H ~>
  // 'G, 'G ~> 'F, 'E ~> 'A, 'E ~> 'F, 'B ~> 'E, 'F ~> 'G, 'B ~> 'F,
  // 'C ~> 'G, 'D ~> 'C, 'H ~> 'D)
  // val edges2 = Seq('I ~> 'M, 'I ~> 'L, 'L ~> 'N, 'M ~> 'N)
  // val edges3 = Seq('P ~> 'O)
  // val graph = Graph.from(edges = edges1 ++ edges2 ++ edges3)

  /** Top axiomatization rules
    *
    * For each concept/role *in the ontology file* introduce a rule to
    * derive `owl:Thing`.
    *
    * @note this might not be enough in cases where data files contain
    * concept/roles that are not in the ontology file. While this is
    * non-standard, it is not forbidden either and may cause problems
    * since not all individuals are considered part of `owl:Thing`.
    *
    * @note this is a naïve implementation of top axiomatization and
    * might change in the future. The ideal solution would be for RDFox
    * to take care of this, but at the time of writing this is not
    * compatible with the way we are using the tool.
    */
  private val topAxioms: List[Rule] = {
    val varX = Variable.create("X")
    val varY = Variable.create("Y")
    val varZ = Variable.create("Z")
    val graph = TupleTableName.create(RSAOntology.CanonGraph.getIRI)
    Rule.create(
      TupleTableAtom.create(graph, varX, IRI.RDF_TYPE, IRI.THING),
      TupleTableAtom.create(graph, varX, IRI.RDF_TYPE, varY)
    ) :: objroles.map(r => {
      val name = r match {
        case x: OWLObjectProperty => x.getIRI.getIRIString
        case x: OWLObjectInverseOf =>
          x.getInverse.getNamedProperty.getIRI.getIRIString :: Inverse
      }
      Rule.create(
        List(
          TupleTableAtom.create(graph, varX, IRI.RDF_TYPE, IRI.THING),
          TupleTableAtom.create(graph, varY, IRI.RDF_TYPE, IRI.THING)
        ),
        List(TupleTableAtom.create(graph, varX, name, varY))
      )
    }) ::: dataroles.map(r => {
      val name = r.getIRI.getIRIString
      Rule.create(
        List(
          TupleTableAtom.create(graph, varX, IRI.RDF_TYPE, IRI.THING),
          TupleTableAtom.create(graph, varY, IRI.RDF_TYPE, IRI.THING)
        ),
        List(TupleTableAtom.create(graph, varX, name, varY))
      )
    })
  }

  /** Equality axiomatization rules
    *
    * Introduce reflexivity, simmetry and transitivity rules for a naïve
    * equality axiomatization.
    *
    * @note that we are using a custom `congruent` predicate to indicate
    * equality. This is to avoid interfering with the standard
    * `owl:sameAs`.
    *
    * @note RDFox is able to handle equality in a "smart" way, but this
    * behaviour is incompatible with other needed features like
    * negation-as-failure and aggregates.
    *
    * @todo naïve substitution rules might not be very efficient. We
    * should look into other ways of implementing this.
    */
  private val equalityAxioms: List[Rule] = {
    val varX = Variable.create("X")
    val varY = Variable.create("Y")
    val varZ = Variable.create("Z")
    val varW = Variable.create("W")
    val graph = TupleTableName.create(RSAOntology.CanonGraph.getIRI)
    // Equality properties
    val properties = List(
      // Reflexivity
      Rule.create(
        TupleTableAtom.create(graph, varX, RSA.CONGRUENT, varX),
        TupleTableAtom.create(graph, varX, IRI.RDF_TYPE, IRI.THING)
      ),
      // Simmetry
      Rule.create(
        TupleTableAtom.create(graph, varY, RSA.CONGRUENT, varX),
        TupleTableAtom.create(graph, varX, RSA.CONGRUENT, varY)
      ),
      // Transitivity
      Rule.create(
        TupleTableAtom.create(graph, varX, RSA.CONGRUENT, varZ),
        TupleTableAtom.create(graph, varX, RSA.CONGRUENT, varY),
        TupleTableAtom.create(graph, varY, RSA.CONGRUENT, varZ)
      )
    )
    /* Equality substitution rules */
    val substitutions =
      Rule.create(
        TupleTableAtom.create(graph, varY, IRI.RDF_TYPE, varZ),
        TupleTableAtom.create(graph, varX, RSA.CONGRUENT, varY),
        TupleTableAtom.create(graph, varX, IRI.RDF_TYPE, varZ)
      ) :: objroles.flatMap(r => {
        val name = r match {
          case x: OWLObjectProperty => x.getIRI.getIRIString
          case x: OWLObjectInverseOf =>
            x.getInverse.getNamedProperty.getIRI.getIRIString :: Inverse
        }
        List(
          Rule.create(
            TupleTableAtom.create(graph, varZ, name, varY),
            TupleTableAtom.create(graph, varX, RSA.CONGRUENT, varZ),
            TupleTableAtom.create(graph, varX, name, varY)
          ),
          Rule.create(
            TupleTableAtom.create(graph, varY, name, varZ),
            TupleTableAtom.create(graph, varX, RSA.CONGRUENT, varZ),
            TupleTableAtom.create(graph, varY, name, varX)
          )
        )
      })
    properties ++ substitutions
  }

  /** Canonical model of the ontology */
  lazy val canonicalModel = Logger.timed(
    new CanonicalModel(this, RSAOntology.CanonGraph),
    "Generating canonical model program",
    Logger.DEBUG
  )

  /** Computes all roles conflicting with a given role
    *
    * @param role a role (object property expression).
    * @return a set of roles conflicting with `role`.
    */
  def confl(
      role: OWLObjectPropertyExpression
  ): Set[OWLObjectPropertyExpression] = {
    reasoner
      .superObjectProperties(role)
      .collect(Collectors.toSet())
      .asScala
      .addOne(role)
      .map(_.getInverseProperty)
      .flatMap(x =>
        reasoner
          .subObjectProperties(x)
          .collect(Collectors.toSet())
          .asScala
          .addOne(x)
      )
      .filterNot(_.isOWLBottomObjectProperty())
      .filterNot(_.getInverseProperty.isOWLTopObjectProperty())
  }

  /** Selfloop detection for a given axiom
    *
    * @param axiom an axiom of type [[OWLSubClassOfAxiom]]
    * @return unfold set for the axiom
    */
  def self(axiom: OWLSubClassOfAxiom): Set[Term] = {
    val role = axiom.objectPropertyExpressionsInSignature(0)
    if (this.confl(role).contains(role)) {
      Set(RSA("v0_" ++ axiom.hashed), RSA("v1_" ++ axiom.hashed))
    } else {
      Set()
    }
  }

  /** Cycle detection for a give axiom
    *
    * @param axiom an axiom of type [[OWLSubClassOfAxiom]]
    * @return unfold set for the axiom
    *
    * @todo we can actually use `toTriple` from `RSAAxiom` to get the
    * classes and the role for a given axiom
    */
  def cycle(axiom: OWLSubClassOfAxiom): Set[Term] = {
    val classes =
      axiom.classesInSignature.collect(Collectors.toList()).asScala
    val classA = classes(0)
    val roleR = axiom
      .objectPropertyExpressionsInSignature(0)
      .asInstanceOf[OWLObjectProperty]
    /* Only one class is returned if A == B */
    val classB = if (classes.length > 1) classes(1) else classes(0)
    cycle_aux(classA, roleR, classB)
  }

  /** Auxiliary function for [[RSAOntology.cycle]] */
  private def cycle_aux(
      classA: OWLClass,
      roleR: OWLObjectProperty,
      classB: OWLClass
  ): Set[Term] = {
    val conflR = this.confl(roleR)
    // TODO: technically we just need the TBox here
    val terms = for {
      axiom1 <- axioms
      if axiom1.isT5
      // We expect only one role coming out of a T5 axiom
      roleS <- axiom1.objectPropertyExpressionsInSignature
      // Triples ordering is among triples involving safe roles.
      if !unsafe.contains(roleS)
      if conflR.contains(roleS)
      tripleARB = RSAAxiom.hashed(classA, roleR, classB)
      tripleDSC = axiom1.hashed
      individual =
        if (tripleARB > tripleDSC) {
          RSA("v1_" ++ tripleDSC)
        } else {
          // Note that this is also the case for
          // `tripleARB == tripleDSC`
          RSA("v0_" ++ tripleDSC)
        }
    } yield individual
    terms to Set
  }

  /** Returns unfold set for self-loop and cycle for the input axiom
    *
    * @param axiom an axiom of type [[OWLSubClassOfAxiom]]
    * @return unfold set for the axiom
    */
  def unfold(axiom: OWLSubClassOfAxiom): Set[Term] =
    this.self(axiom) | this.cycle(axiom)

  /** Returns the answers to a single query
    *
    *  @param queries a sequence of conjunctive queries to answer.
    *  @return a collection of answers for each query.
    */
  def ask(query: ConjunctiveQuery): ConjunctiveQueryAnswers = this._ask(query)

  /** Returns the answers to a collection of queries
    *
    *  @param queries a sequence of conjunctive queries to answer.
    *  @return a collection of answers for each query.
    */
  def ask(queries: Seq[ConjunctiveQuery]): Seq[ConjunctiveQueryAnswers] =
    queries map _ask

  private lazy val _ask: ConjunctiveQuery => ConjunctiveQueryAnswers = {
    val (server, data) = RDFoxUtil.openConnection(RSAOntology.DataStore)

    /* Upload data from data file */
    RDFoxUtil.addData(data, RSAOntology.CanonGraph, datafiles: _*)

    /* Top/equality axiomatization */
    RDFoxUtil.updateData(data, 
      s"""
      INSERT { 
        GRAPH ${RSAOntology.CanonGraph} { ?X a ${IRI.THING} }
      } WHERE {
        GRAPH ${RSAOntology.CanonGraph} { ?X ?Y ?Z }
      }
      """
    )
    RDFoxUtil.updateData(data, 
      s"""
      INSERT { 
        GRAPH ${RSAOntology.CanonGraph} { ?Z a ${RSA.NAMED} }
      } WHERE {
        GRAPH ${RSAOntology.CanonGraph} { ?X ?Y ?Z }.
        FILTER( ?Y != a )
      }
      """
    )
    RDFoxUtil.addRules(data, topAxioms ++ equalityAxioms)
    Logger.write(topAxioms.mkString("\n"), "axiomatisation.dlog")
    Logger.write(equalityAxioms.mkString("\n"), "axiomatisation.dlog")

    /* Introduce `rsacomb:Named` concept */
    /* From data */
    RDFoxUtil.updateData(data, 
      s"""
      INSERT { 
        GRAPH ${RSAOntology.CanonGraph} { ?X a ${RSA.NAMED} }
      } WHERE {
        GRAPH ${RSAOntology.CanonGraph} { ?X a ${IRI.THING} }
      }
      """
    )
    /* From ontology */
    val named = individuals.map(RSA.Named(RSAOntology.CanonGraph)(_))
    RDFoxUtil.addFacts(data, RSAOntology.CanonGraph, named)
    if (! named.isEmpty)
      Logger.write(named.mkString("", ".\n", ".\n"), "canonical_model.dlog")

    /* Add canonical model */
    Logger print s"Canonical model facts: ${this.canonicalModel.facts.length}"
    if (! canonicalModel.facts.isEmpty)
      Logger.write(canonicalModel.facts.mkString("", ".\n", ".\n"), "canonical_model.dlog")
    RDFoxUtil.addFacts(data, RSAOntology.CanonGraph, this.canonicalModel.facts)
    Logger print s"Canonical model rules: ${this.canonicalModel.rules.length}"
    Logger.write(canonicalModel.rules.mkString("\n"), "canonical_model.dlog")
    RDFoxUtil.addRules(data, this.canonicalModel.rules)

    /* Finalise canonical model */
    data.clearRulesAxiomsExplicateFacts()

    RDFoxUtil.closeConnection(server, data)

    (query => {
      Logger print s"Query ID: ${query.id}"

      val (server, data) = RDFoxUtil.openConnection(RSAOntology.DataStore)

      val filter = RSAOntology.filteringProgram(query)

      /* Add filtering program */
      Logger print s"Filtering program rules: ${filter.rules.length}"
      Logger.write(
        filter.rules.mkString("\n"),
        s"filter_query_${query.id}.dlog"
      )
      RDFoxUtil.addRules(data, filter.rules)

      /* Gather answers to the query */
      val answers = RDFoxUtil
        .submitQuery(data, filter.answerQuery, RSA.Prefixes)
        .map(new ConjunctiveQueryAnswers(query, query.variables, _))
        .get

      /* Drop filtering named graph to avoid running out of memory */
      data.clearRulesAxiomsExplicateFacts()
      data.deleteTupleTable(filter.target.getIRI)

      RDFoxUtil.closeConnection(server, data)

      Logger print s"Number of answers: ${answers.length}"

      answers
    })
  }

  /** Query the RDFox data store used for query answering.
    *
    * @note This method does not add any facts or rules to the data
    * store. It is most useful after the execution of a query using
    * [[RSAOntology.ask]].
    *
    * @param query query to be executed against the environment
    * @param prefixes additional prefixes for the query. It defaults to
    * an empty set.
    * @param opts additional options to RDFox.
    * @return a collection of answers to the input query.
    *
    * @note This method has been introduced mostly for debugging purposes.
    */
  def queryDataStore(
      query: String,
      prefixes: Prefixes = new Prefixes(),
      opts: ju.Map[String, String] = new ju.HashMap[String, String]()
  ): Option[Seq[(Long, Seq[Resource])]] = {
    val (server, data) = RDFoxUtil.openConnection(RSAOntology.DataStore)
    val answers = RDFoxUtil.submitQuery(data, query, prefixes, opts)
    RDFoxUtil.closeConnection(server, data)
    answers
  }

  /** Returns set of unfiltered answers.
    *
    * This is equivalent to quering just the canonical model.
    *
    * @note this method does not load any data to RDFox. The return
    * value is considered well defined only after
    * [[uk.ac.ox.cs.rsacomb.RSAOntology.ask RSAOntology.ask]]
    * for the corresponding query has been called.
    */
  // def askUnfiltered(
  //     cq: ConjunctiveQuery
  // ): Option[Seq[(Long, Seq[Resource])]] = {
  //   val query = RDFoxUtil.buildDescriptionQuery("QM", cq.variables.length)
  //   queryDataStore(query, RSA.Prefixes)
  // }

}
