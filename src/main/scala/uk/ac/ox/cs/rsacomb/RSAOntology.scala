package uk.ac.ox.cs.rsacomb

/* Java imports */
import java.{util => ju}
import java.util.HashMap
import java.util.stream.{Collectors, Stream}
import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.util.OWLOntologyMerger
import org.semanticweb.owlapi.model.{OWLOntology, OWLAxiom}
import org.semanticweb.owlapi.model.{
  OWLClass,
  OWLObjectProperty,
  OWLSubObjectPropertyOfAxiom,
  OWLObjectPropertyExpression,
  OWLObjectSomeValuesFrom,
  OWLSubClassOfAxiom
}
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.model.{IRI => OWLIRI}
import uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl

import tech.oxfordsemantic.jrdfox.client.{UpdateType, DataStoreConnection}
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
  Resource
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

/* Scala imports */
import scala.collection.JavaConverters._
import scala.collection.mutable.Set
import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge.UnDiEdge

/* Debug only */
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer
import tech.oxfordsemantic.jrdfox.logic._
import org.semanticweb.owlapi.model.OWLObjectInverseOf

import uk.ac.ox.cs.rsacomb.converter.{RDFoxAxiomConverter, SkolemStrategy}
import uk.ac.ox.cs.rsacomb.suffix._
import uk.ac.ox.cs.rsacomb.sparql._
import uk.ac.ox.cs.rsacomb.util.{RDFoxUtil, RSA}

object RSAOntology {

  // Counter used to implement a simple fresh variable generator
  private var counter = -1;

  /** Name of the RDFox data store used for CQ answering */
  private val DataStore = "answer_computation"

  def apply(ontology: OWLOntology): RSAOntology = new RSAOntology(ontology)

  def apply(ontologies: File*): RSAOntology =
    new RSAOntology(loadOntology(ontologies: _*))

  def genFreshVariable(): Variable = {
    counter += 1
    Variable.create(f"I$counter%03d")
  }

  private def loadOntology(ontologies: File*): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()
    ontologies.foreach { manager.loadOntologyFromOntologyDocument(_) }
    val merger = new OWLOntologyMerger(manager)
    merger.createMergedOntology(manager, OWLIRI.create("_:merged"))
  }
}

class RSAOntology(val ontology: OWLOntology) {

  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._

  // Gather TBox/RBox/ABox from original ontology
  val tbox: List[OWLAxiom] =
    ontology
      .tboxAxioms(Imports.INCLUDED)
      .collect(Collectors.toList())
      .asScala
      .toList

  val rbox: List[OWLAxiom] =
    ontology
      .rboxAxioms(Imports.INCLUDED)
      .collect(Collectors.toList())
      .asScala
      .toList

  val abox: List[OWLAxiom] =
    ontology
      .aboxAxioms(Imports.INCLUDED)
      .collect(Collectors.toList())
      .asScala
      .toList

  val axioms: List[OWLAxiom] = abox ::: tbox ::: rbox

  /* Retrieve individuals in the original ontology
   */
  val individuals: List[IRI] =
    ontology
      .getIndividualsInSignature()
      .asScala
      .map(_.getIRI)
      .map(implicits.RDFox.owlapiToRdfoxIri)
      .toList

  val concepts: List[OWLClass] =
    ontology.getClassesInSignature().asScala.toList

  val roles: List[OWLObjectPropertyExpression] =
    axioms
      .flatMap(_.objectPropertyExpressionsInSignature)
      .distinct

  /** OWLAPI reasoner
    *
    * Used to carry out some preliminary reasoning task.
    */
  private val reasoner =
    (new StructuralReasonerFactory()).createReasoner(ontology)

  /* Steps for RSA check
   * 1) convert ontology axioms into LP rules
   * 2) call RDFox on the onto and compute materialization
   * 3) build graph from E(x,y) facts
   * 4) check if the graph is tree-like
   *    ideally this annotates the graph with info about the reasons
   *    why the ontology might not be RSA. This could help a second
   *    step of approximation of an Horn-ALCHOIQ to RSA
   */
  lazy val isRSA: Boolean = {

    val unsafe = this.unsafeRoles

    /* DEBUG: print rules in DL syntax and unsafe roles */
    //val renderer = new DLSyntaxObjectRenderer()
    //println("\nDL rules:")
    //axioms.foreach(x => println(renderer.render(x)))
    //println("\nUnsafe roles:")
    //println(unsafe)

    /* Ontology convertion into LP rules */
    val datalog = for {
      axiom <- axioms
      visitor = new RDFoxAxiomConverter(
        RSAOntology.genFreshVariable(),
        unsafe,
        SkolemStrategy.ConstantRSA(axiom.toString),
        Empty
      )
      rule <- axiom.accept(visitor)
    } yield rule

    /* DEBUG: print datalog rules */
    println("\nDatalog roles:")
    datalog.foreach(println)

    // Open connection with RDFox
    val (server, data) = RDFoxUtil.openConnection("RSACheck")
    // Add Data (hardcoded for now)
    //data.importData(UpdateType.ADDITION, RSA.Prefixes, ":a a :A .")

    /* Add built-in rules
     */
    data.importData(
      UpdateType.ADDITION,
      RSA.Prefixes,
      "rsa:E[?X,?Y] :- rsa:PE[?X,?Y], rsa:U[?X], rsa:U[?Y] ."
    )

    /* Add built-in rules
     */
    // data.importData(
    //   UpdateType.ADDITION,
    //   RSA.Prefixes,
    //   "[?entity, a, ?superClass] :- [?entity, a, ?class], [?class, rdfs:subClassOf, ?superClass] ."
    // )

    /* Add ontology rules
     */
    data.addRules(datalog.asJava)

    /* Build graph
     */
    val graph = this.rsaGraph(data);
    //println(graph)

    // Close connection to RDFox
    RDFoxUtil.closeConnection(server, data)

    /* To check if the graph is tree-like we check for acyclicity in a
     * undirected graph.
     *
     * TODO: Implement additional checks (taking into account equality)
     */
    graph.isAcyclic
  }

  lazy val unsafeRoles: List[OWLObjectPropertyExpression] = {

    /* DEBUG: print rules in DL syntax */
    //val renderer = new DLSyntaxObjectRenderer()

    /* Checking for (1) unsafety condition:
     *
     *    For all roles r1 appearing in an axiom of type T5, r1 is unsafe
     *    if there exists a role r2 (different from top) appearing in an axiom
     *    of type T3 and r1 is a subproperty of the inverse of r2.
     */
    val unsafe1 = for {
      axiom <- tbox
      if axiom.isT5
      role1 <- axiom.objectPropertyExpressionsInSignature
      roleSuper =
        role1 +: reasoner
          .superObjectProperties(role1)
          .collect(Collectors.toList())
          .asScala
      roleSuperInv = roleSuper.map(_.getInverseProperty)
      axiom <- tbox
      if axiom.isT3 && !axiom.isT3top
      role2 <- axiom.objectPropertyExpressionsInSignature
      if roleSuperInv.contains(role2)
    } yield role1

    /* Checking for (2) unsafety condition:
     *
     *    For all roles p1 appearing in an axiom of type T5, p1 is unsafe if
     *    there exists a role p2 appearing in an axiom of type T4 and p1 is a
     *    subproperty of either p2 or the inverse of p2.
     *
     */
    val unsafe2 = for {
      axiom <- tbox
      if axiom.isT5
      role1 <- axiom.objectPropertyExpressionsInSignature
      roleSuper =
        role1 +: reasoner
          .superObjectProperties(role1)
          .collect(Collectors.toList())
          .asScala
      roleSuperInv = roleSuper.map(_.getInverseProperty)
      axiom <- tbox
      if axiom.isT4
      role2 <- axiom.objectPropertyExpressionsInSignature
      if roleSuper.contains(role2) || roleSuperInv.contains(role2)
    } yield role1

    (unsafe1 ++ unsafe2).toList
  }

  private def rsaGraph(
      data: DataStoreConnection
  ): Graph[Resource, UnDiEdge] = {
    val query = "SELECT ?X ?Y WHERE { ?X rsa:E ?Y }"
    val answers = RDFoxUtil.submitQuery(data, query, RSA.Prefixes).get
    var edges: Seq[UnDiEdge[Resource]] = answers.map {
      case Seq(n1, n2) => UnDiEdge(n1, n2)
    }
    Graph(edges: _*)
  }

  def filteringProgram(query: ConjunctiveQuery): FilteringProgram =
    new FilteringProgram(query, individuals)

  lazy val canonicalModel = new CanonicalModel(this)

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
  def ask(query: ConjunctiveQuery): ConjunctiveQueryAnswers = {
    import implicits.JavaCollections._
    val (server, data) = RDFoxUtil.openConnection(RSAOntology.DataStore)
    data.addRules(this.canonicalModel.rules)
    data.addRules(this.filteringProgram(query).rules)
    val answers = RDFoxUtil
      .submitQuery(
        data,
        RDFoxUtil.buildDescriptionQuery("Ans", query.answer.size),
        RSA.Prefixes
      )
      .map(
        new ConjunctiveQueryAnswers(query.bcq, _)
      )
      .get
    RDFoxUtil.closeConnection(server, data)
    answers
  }

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
  ): Option[Seq[Seq[Resource]]] = {
    val (server, data) = RDFoxUtil.openConnection(RSAOntology.DataStore)
    val answers = RDFoxUtil.submitQuery(data, query, prefixes, opts)
    RDFoxUtil.closeConnection(server, data)
    answers
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

  // def cycle(axiom: OWLSubClassOfAxiom): Set[Term] = {
  //   // Assuming just one role in the signature of a T5 axiom
  //   val roleR = axiom.objectPropertyExpressionsInSignature(0)
  //   val conflR = this.confl(roleR)
  //   // We just need the TBox to find
  //   val tbox = ontology
  //     .tboxAxioms(Imports.INCLUDED)
  //     .collect(Collectors.toSet())
  //     .asScala
  //   for {
  //     axiom1 <- tbox
  //     // TODO: is this an optimization or an error?
  //     if axiom1.isT5
  //     // We expect only one role coming out of a T5 axiom
  //     roleS <- axiom1.objectPropertyExpressionsInSignature
  //     // Triples ordering is among triples involving safe roles.
  //     if !unsafeRoles.contains(roleS)
  //     if conflR.contains(roleS)
  //     individual =
  //       if (axiom.hashCode < axiom1.hashCode) {
  //         RSA.rsa("v0_" ++ axiom1.hashCode.toString())
  //       } else {
  //         RSA.rsa("v1_" ++ axiom1.hashCode.toString())
  //       }
  //   } yield individual
  // }

  def cycle(axiom: OWLSubClassOfAxiom): Set[Term] = {
    // TODO: we can actually use `toTriple` from `RSAAxiom`
    val classes =
      axiom.classesInSignature.collect(Collectors.toList()).asScala
    val classA = classes(0)
    val roleR = axiom
      .objectPropertyExpressionsInSignature(0)
      .asInstanceOf[OWLObjectProperty]
    val classB = classes(1)
    cycle_aux(classA, roleR, classB)
  }

  def cycle_aux(
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

  def unfold(axiom: OWLSubClassOfAxiom): Set[Term] =
    this.self(axiom) | this.cycle(axiom)

} // implicit class RSAOntology
