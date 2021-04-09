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
  Rule,
  TupleTableAtom,
  Negation,
  BodyFormula
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  Term,
  Variable,
  IRI,
  Resource,
  Literal
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

/* Scala imports */
import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._
import scala.collection.mutable.{Set, Map}
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.GraphTraversal._

/* Debug only */
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer
import tech.oxfordsemantic.jrdfox.logic._
import org.semanticweb.owlapi.model.OWLObjectInverseOf

import uk.ac.ox.cs.rsacomb.converter._
import uk.ac.ox.cs.rsacomb.filtering.{FilteringProgram, FilterType}
import uk.ac.ox.cs.rsacomb.suffix._
import uk.ac.ox.cs.rsacomb.sparql._
import uk.ac.ox.cs.rsacomb.util.{RDFoxUtil, RSA}
import uk.ac.ox.cs.rsacomb.util.Logger

object RSAOntology {

  /** Name of the RDFox data store used for CQ answering */
  private val DataStore = "answer_computation"

  /** Simple fresh variable generator */
  private var counter = -1;
  def genFreshVariable(): Variable = {
    counter += 1
    Variable.create(f"I$counter%05d")
  }

  /** Manager instance to interface with OWLAPI */
  val manager = OWLManager.createOWLOntologyManager()

  def apply(ontology: File, data: File*): RSAOntology =
    new RSAOntology(
      manager.loadOntologyFromOntologyDocument(ontology),
      data: _*
    )

  def apply(ontology: OWLOntology, data: File*): RSAOntology =
    new RSAOntology(ontology, data: _*)
}

/** Wrapper class for an ontology in RSA
  *
  * @param ontology the input OWL2 ontology.
  * @param datafiles additinal data (treated as part of the ABox)
  */
class RSAOntology(val original: OWLOntology, val datafiles: File*) {

  /** Simplify conversion between OWLAPI and RDFox concepts */
  import implicits.RDFox._
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Set of axioms removed during the approximation to RSA */
  private var removed: Seq[OWLAxiom] = Seq.empty

  /** The normalizer normalizes the ontology and approximate it to
    * Horn-ALCHOIQ. A further step is needed to obtain an RSA
    * approximation of the input ontology `original`.
    */
  private val normalizer = new Normalizer()

  /** TBox axioms */
  var tbox: List[OWLLogicalAxiom] =
    original
      .tboxAxioms(Imports.INCLUDED)
      .collect(Collectors.toList())
      .collect { case a: OWLLogicalAxiom => a }
      .flatMap(normalizer.normalize)

  /** RBox axioms */
  var rbox: List[OWLLogicalAxiom] =
    original
      .rboxAxioms(Imports.INCLUDED)
      .collect(Collectors.toList())
      .collect { case a: OWLLogicalAxiom => a }
      .flatMap(normalizer.normalize)

  /** ABox axioms
    *
    * @note this represents only the set of assertions contained in the
    * ontology file. Data files specified in `datafiles` are directly
    * imported in RDFox due to performance issues when trying to import
    * large data files via OWLAPI.
    */
  var abox: List[OWLLogicalAxiom] =
    original
      .aboxAxioms(Imports.INCLUDED)
      .collect(Collectors.toList())
      .collect { case a: OWLLogicalAxiom => a }
      .flatMap(normalizer.normalize)

  /** Collection of logical axioms in the input ontology */
  var axioms: List[OWLLogicalAxiom] = abox ::: tbox ::: rbox

  /** Normalized Horn-ALCHOIQ ontology */
  val ontology = RSAOntology.manager.createOntology(
    axioms.asInstanceOf[List[OWLAxiom]].asJava
  )

  /** OWLAPI internal reasoner instantiated over the approximated ontology */
  private val reasoner =
    (new StructuralReasonerFactory()).createReasoner(ontology)

  /** Retrieve individuals/literals in the ontology */
  val individuals: List[IRI] =
    ontology
      .getIndividualsInSignature()
      .asScala
      .map(_.getIRI)
      .map(implicits.RDFox.owlapiToRdfoxIri)
      .toList
  val literals: List[Literal] =
    axioms
      .collect { case a: OWLDataPropertyAssertionAxiom => a }
      .map(_.getObject)
      .map(implicits.RDFox.owlapiToRdfoxLiteral)

  /** Retrieve concepts/roles in the ontology */
  val concepts: List[OWLClass] =
    ontology.getClassesInSignature().asScala.toList
  val roles: List[OWLObjectPropertyExpression] =
    (tbox ++ rbox)
      .flatMap(_.objectPropertyExpressionsInSignature)
      .distinct

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
  val unsafeRoles: List[OWLObjectPropertyExpression] = {

    /* Checking for unsafety condition (1) */
    val unsafe1 = for {
      axiom <- tbox
      if axiom.isT5
      role1 <- axiom.objectPropertyExpressionsInSignature
      roleSuper = role1 +: reasoner.superObjectProperties(role1)
      roleSuperInv = roleSuper.map(_.getInverseProperty)
      axiom <- tbox
      if axiom.isT3 && !axiom.isT3top
      role2 <- axiom.objectPropertyExpressionsInSignature
      if roleSuperInv contains role2
    } yield role1

    /* Checking for unsafety condition (2) */
    val unsafe2 = for {
      axiom <- tbox
      if axiom.isT5
      role1 <- axiom.objectPropertyExpressionsInSignature
      roleSuper = role1 +: reasoner.superObjectProperties(role1)
      roleSuperInv = roleSuper.map(_.getInverseProperty)
      axiom <- tbox
      if axiom.isT4
      role2 <- axiom.objectPropertyExpressionsInSignature
      if roleSuper.contains(role2) || roleSuperInv.contains(role2)
    } yield role1

    unsafe1 ++ unsafe2
  }

  /** Compute the RSA dependency graph
    *
    * This is used to approximate the input ontology to RSA.
    *
    * @return a tuple containing the dependency graph and a map between
    * the constants newly introduced and the corresponding axioms in the
    * ontology.
    */
  private def dependencyGraph()
      : (Graph[Resource, DiEdge], Map[String, OWLAxiom]) = {
    val unsafe = this.unsafeRoles
    var nodemap = Map.empty[String, OWLAxiom]

    object RSAConverter extends RDFoxConverter {

      override def convert(
          expr: OWLClassExpression,
          term: Term,
          unsafe: List[OWLObjectPropertyExpression],
          skolem: SkolemStrategy,
          suffix: RSASuffix
      ): Shards =
        (expr, skolem) match {

          case (e: OWLObjectSomeValuesFrom, c: Constant) => {
            nodemap.update(c.iri.getIRI, c.axiom)
            val (res, ext) = super.convert(e, term, unsafe, skolem, suffix)
            if (unsafe contains e.getProperty)
              (RSA.PE(term, c.iri) :: RSA.U(c.iri) :: res, ext)
            else
              (RSA.PE(term, c.iri) :: res, ext)
          }

          case (e: OWLDataSomeValuesFrom, c: Constant) => {
            nodemap.update(c.iri.getIRI, c.axiom)
            val (res, ext) = super.convert(e, term, unsafe, skolem, suffix)
            if (unsafe contains e.getProperty)
              (RSA.PE(term, c.iri) :: RSA.U(c.iri) :: res, ext)
            else
              (RSA.PE(term, c.iri) :: res, ext)
          }

          case _ => super.convert(expr, term, unsafe, skolem, suffix)
        }
    }

    /* Ontology convertion into LP rules */
    val term = RSAOntology.genFreshVariable()
    val result = axioms.map(a =>
      RSAConverter.convert(a, term, unsafe, new Constant(a), Empty)
    )

    val datalog = result.unzip
    val facts = datalog._1.flatten
    var rules = datalog._2.flatten

    /* Open connection with RDFox */
    val (server, data) = RDFoxUtil.openConnection("rsa_dependency_graph")

    /* Add additional built-in rules */
    val varX = Variable.create("X")
    val varY = Variable.create("Y")
    rules = Rule.create(
      RSA.E(varX, varY),
      RSA.PE(varX, varY),
      RSA.U(varX),
      RSA.U(varY)
    ) :: rules
    /* Load facts and rules from ontology */
    RDFoxUtil.addFacts(data, facts)
    RDFoxUtil.addRules(data, rules)
    /* Load data files */
    RDFoxUtil.addData(data, datafiles: _*)

    /* Build the graph */
    val query = "SELECT ?X ?Y WHERE { ?X rsa:E ?Y }"
    val answers = RDFoxUtil.submitQuery(data, query, RSA.Prefixes).get
    var edges: Seq[DiEdge[Resource]] =
      answers.collect { case (_, Seq(n1, n2)) => n1 ~> n2 }
    val graph = Graph(edges: _*)

    /* Close connection to RDFox */
    RDFoxUtil.closeConnection(server, data)

    (graph, nodemap)
  }

  /** Approximate a Horn-ALCHOIQ ontology to RSA
    *
    * This is done by gathering those axioms that prevent the ontology
    * dependency graph `dependencyGraph` from being tree-shaped, and
    * removing them.
    *
    * @param graph the graph used to compute the axioms to remove.
    * @param nodemap map from graph nodes to ontology axioms.
    */
  def toRSA(): RSAOntology = Logger.timed(
    {

      /* Compute the dependency graph for the ontology */
      val (graph, nodemap) = this.dependencyGraph()

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
      }.toSeq

      /* Remove axioms from approximated ontology */
      ontology.removeAxioms(toDelete: _*)
      this.removed = toDelete

      /* Return RSA ontology */
      RSAOntology(ontology, datafiles: _*)
    },
    "Horn-ALCHOIQ to RSA approximation:",
    Logger.DEBUG
  )
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
    concepts
      .map(c => {
        Rule.create(
          RSA.Thing(varX),
          TupleTableAtom.rdf(varX, IRI.RDF_TYPE, c.getIRI)
        )
      }) ++ roles.map(r => {
      val name = r match {
        case x: OWLObjectProperty => x.getIRI.getIRIString
        case x: OWLObjectInverseOf =>
          x.getInverse.getNamedProperty.getIRI.getIRIString :: Inverse
      }
      Rule.create(
        List(RSA.Thing(varX), RSA.Thing(varY)),
        List(TupleTableAtom.rdf(varX, name, varY))
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
    * @todo to complete the equality axiomatization we need to introduce
    * substitution rules to explicate a complete "equality" semantics.
    */
  private val equalityAxioms: List[Rule] = {
    val varX = Variable.create("X")
    val varY = Variable.create("Y")
    val varZ = Variable.create("Z")
    List(
      // Reflexivity
      Rule.create(RSA.Congruent(varX, varX), RSA.Thing(varX)),
      // Simmetry
      Rule.create(RSA.Congruent(varY, varX), RSA.Congruent(varX, varY)),
      // Transitivity
      Rule.create(
        RSA.Congruent(varX, varZ),
        RSA.Congruent(varX, varY),
        RSA.Congruent(varY, varZ)
      )
    )
  }

  lazy val canonicalModel = Logger.timed(
    new CanonicalModel(this),
    "Generating canonical model program",
    Logger.DEBUG
  )

  def filteringProgram(query: ConjunctiveQuery): FilteringProgram =
    Logger.timed(
      FilteringProgram(FilterType.REVISED)(query),
      "Generating filtering program",
      Logger.DEBUG
    )

  // TODO: the following functions needs testing
  def confl(
      role: OWLObjectPropertyExpression
  ): Set[OWLObjectPropertyExpression] = {

    val invSuperRoles = reasoner
      .superObjectProperties(role)
      .collect(Collectors.toSet())
      .asScala
      .addOne(role)
      .map(_.getInverseProperty)

    invSuperRoles
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

  /** Returns the answers to a query
    *
    *  @param query query to execute
    *  @return a collection of answers
    */
  def ask(query: ConjunctiveQuery): ConjunctiveQueryAnswers = Logger.timed(
    {
      import implicits.JavaCollections._
      val (server, data) = RDFoxUtil.openConnection(RSAOntology.DataStore)
      val canon = this.canonicalModel
      val filter = this.filteringProgram(query)

      /* Upload data from data file */
      RDFoxUtil.addData(data, datafiles: _*)

      RDFoxUtil printStatisticsFor data

      /* Top / equality axiomatization */
      RDFoxUtil.addRules(data, topAxioms ++ equalityAxioms)

      /* Generate `named` predicates */
      RDFoxUtil.addFacts(data, (individuals ++ literals) map RSA.Named)
      data.evaluateUpdate(
        RSA.Prefixes,
        "INSERT { ?X a  rsa:Named } WHERE { ?X a owl:Thing }",
        new java.util.HashMap[String, String]
      )

      Logger print s"Canonical model rules: ${canon.rules.length}"
      RDFoxUtil.addRules(data, canon.rules)

      Logger print s"Canonical model facts: ${canon.facts.length}"
      RDFoxUtil.addFacts(data, canon.facts)

      //{
      //  import java.io.{PrintStream, FileOutputStream, File}
      //  val rules1 = new FileOutputStream(new File("rules1-lubm200.dlog"))
      //  val facts1 = new FileOutputStream(new File("facts1-lubm200.ttl"))
      //  RDFoxUtil.export(data, rules1, facts1)
      //  val rules2 = new PrintStream(new File("rules2-q34.dlog"))
      //  rules2.print(filter.rules.mkString("\n"))
      //}

      //canon.facts.foreach(println)
      //filter.rules.foreach(println)

      RDFoxUtil printStatisticsFor data

      Logger print s"Filtering program rules: ${filter.rules.length}"
      RDFoxUtil.addRules(data, filter.rules)

      RDFoxUtil printStatisticsFor data

      val answers = {
        val ans = filter.answerQuery
        RDFoxUtil
          .submitQuery(data, ans, RSA.Prefixes)
          .map(new ConjunctiveQueryAnswers(query.bcq, query.variables, _))
          .get
      }
      RDFoxUtil.closeConnection(server, data)
      answers
    },
    "Answers computation",
    Logger.DEBUG
  )

  /** Query the RDFox data store used for query answering.
    *
    * @note This method does not add any facts or rules to the data
    * store. It is most useful after the execution of a query using
    * [[uk.ac.ox.cs.rsacomb.RSAOntology.ask RSAOntology.ask]].
    * @note This method has been introduced mostly for debugging purposes.
    *
    * @param cq a CQ used to compute the environment.
    * @param query query to be executed against the environment
    * @param prefixes additional prefixes for the query. It defaults to
    * an empty set.
    * @param opts additional options to RDFox.
    * @return a collection of answers to the input query.
    */
  def queryDataStore(
      cq: ConjunctiveQuery,
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
  def askUnfiltered(
      cq: ConjunctiveQuery
  ): Option[Seq[(Long, Seq[Resource])]] = {
    val query = RDFoxUtil.buildDescriptionQuery("QM", cq.variables.length)
    queryDataStore(cq, query, RSA.Prefixes)
  }

  def self(axiom: OWLSubClassOfAxiom): Set[Term] = {
    // Assuming just one role in the signature of a T5 axiom
    val role = axiom.objectPropertyExpressionsInSignature(0)
    if (this.confl(role).contains(role)) {
      Set(
        RSA("v0_" ++ axiom.hashed),
        RSA("v1_" ++ axiom.hashed)
      )
    } else {
      Set()
    }
  }

  def cycle(axiom: OWLSubClassOfAxiom): Set[Term] = {
    // TODO: we can actually use `toTriple` from `RSAAxiom`
    val classes =
      axiom.classesInSignature.collect(Collectors.toList()).asScala
    val classA = classes(0)
    val roleR = axiom
      .objectPropertyExpressionsInSignature(0)
      .asInstanceOf[OWLObjectProperty]
    val classB = classes(1)
    cycle_aux1(classA, roleR, classB)
  }

  def cycle_aux0(
      classA: OWLClass,
      roleR: OWLObjectProperty,
      classB: OWLClass
  ): Set[Term] = {
    val conflR = this.confl(roleR)
    val classes = ontology
      .classesInSignature(Imports.INCLUDED)
      .collect(Collectors.toSet())
      .asScala
    for {
      classD <- classes
      roleS <- conflR
      classC <- classes
      // Keeping this check for now
      if !unsafeRoles.contains(roleS)
      tripleARB = RSAAxiom.hashed(classA, roleR, classB)
      tripleDSC = RSAAxiom.hashed(classD, roleS, classC)
      individual =
        if (tripleARB > tripleDSC) {
          RSA("v1_" ++ tripleDSC)
        } else {
          // Note that this is also the case for
          // `tripleARB == tripleDSC`
          RSA("v0_" ++ tripleDSC)
        }
    } yield individual
  }

  def cycle_aux1(
      classA: OWLClass,
      roleR: OWLObjectProperty,
      classB: OWLClass
  ): Set[Term] = {
    val conflR = this.confl(roleR)
    // We just need the TBox to find
    val terms = for {
      axiom1 <- tbox
      // TODO: is this an optimization or an error?
      if axiom1.isT5
      // We expect only one role coming out of a T5 axiom
      roleS <- axiom1.objectPropertyExpressionsInSignature
      // Triples ordering is among triples involving safe roles.
      if !unsafeRoles.contains(roleS)
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

  def unfold(axiom: OWLSubClassOfAxiom): Set[Term] =
    this.self(axiom) | this.cycle(axiom)

  /** Log normalization/approximation statistics */
  def statistics(level: Logger.Level = Logger.DEBUG): Unit = {
    Logger.print(
      s"Logical axioms in original input ontology: ${original.getLogicalAxiomCount(true)}",
      level
    )
    Logger.print(
      s"Logical axioms discarded in Horn-ALCHOIQ approximation: ${normalizer.discarded}",
      level
    )
    Logger.print(
      s"Logical axioms shifted in Horn-ALCHOIQ approximation: ${normalizer.shifted}",
      level
    )
    Logger.print(
      s"Logical axioms in Horn-ALCHOIQ ontology: ${ontology
        .getLogicalAxiomCount(true)} (${tbox.length}/${rbox.length}/${abox.length})",
      level
    )
    Logger.print(
      s"Logical axioms discarded in RSA approximation: ${removed.length}",
      level
    )
  }

} // class RSAOntology
