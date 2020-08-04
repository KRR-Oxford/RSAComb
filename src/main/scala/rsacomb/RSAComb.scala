package rsacomb

import java.io.File
import java.util.HashMap
import java.util.stream.{Stream,Collectors}

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AxiomType, ClassExpressionType, OWLObjectSomeValuesFrom}
import org.semanticweb.owlapi.model.{OWLAxiom, OWLSubClassOfAxiom, OWLEquivalentClassesAxiom}
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.parameters.Imports
import uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl

import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.{ConnectionFactory, ServerConnection, DataStoreConnection}
import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.{Rule, Atom, Literal, Term, Variable}
import tech.oxfordsemantic.jrdfox.logic.{BuiltinFunctionCall, TupleTableName}
import tech.oxfordsemantic.jrdfox.logic.{LogicFormat}

import scala.collection.JavaConverters._

import rsacomb.SkolemStrategy
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer

class RSA(ontology : OWLOntology) {

  /* Alternative constructor(s) */
  def this(file : File) = this(RSA.loadOntology(file))

  def getOntology : OWLOntology = ontology

} // class RSA

object RSA {

  def loadOntology( onto : File ) : OWLOntology = {
    /* Retrieve ontology manager */
    val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager()
    /* Retrieve ontology */
    manager.loadOntologyFromOntologyDocument(onto)
  }

  def isRSA( onto : OWLOntology ) : Boolean = {
    /* TODO: Steps for RSA check
     * 1) convert ontology axioms into LP rules
     * 2) call RDFox on the onto and compute materialization
     * 3) build graph from E(x,y) facts
     * 4) check if the graph is tree-like
     *    ideally this annotates the graph with info about the reasons
     *    why the ontology might not be RSA. This could help a second
     *    step of approximation of an Horn-ALCHOIQ to RSA
     */

    val renderer = new DLSyntaxObjectRenderer()

    // Here we need to compute the unsafe roles. This is hardcoded for now.
    val unsafe = List(
      new OWLObjectPropertyImpl(IRI.create("http://example.com/rsa_example.owl#S")).getInverseProperty()
    )

    /* Print TBox axioms */
    println("TBox/RBox:")
    for {
      axiom <- onto.tboxAxioms(Imports.EXCLUDED).collect(Collectors.toList()).asScala
    } yield println(renderer.render(axiom))
    for {
      axiom <- onto.rboxAxioms(Imports.EXCLUDED).collect(Collectors.toList()).asScala
    } yield println(renderer.render(axiom))

    /* Ontology axiom convertion into LP rules */
    println("Logic rules:")
    for {
      axiom <- onto.tboxAxioms(Imports.EXCLUDED).collect(Collectors.toList()).asScala
      visitor = new RDFoxAxiomConverter(Variable.create("x"), SkolemStrategy.ConstantRSA(axiom.toString), unsafe)
      rule  <- axiom.accept(visitor)
    } yield println(rule)
    for {
      axiom <- onto.rboxAxioms(Imports.EXCLUDED).collect(Collectors.toList()).asScala
      visitor = new RDFoxAxiomConverter(Variable.create("x"), SkolemStrategy.ConstantRSA(axiom.toString), unsafe)
      rule  <- axiom.accept(visitor)
    } yield println(rule)

    /* Return true for now... */
    true
  }

} // object RSA

object RSAComb {

  val help : String = """
      rsacomb - combined approach for CQ answering for RSA ontologies.

      USAGE
        rsacomb <path/to/ontology.owl> <path/to/query.sparql>

      where
         the ontology is expected to be an OWL file and the (single)
         query a SPARQL query file.
  """

  def main( args : Array[String] ) : Unit = {

    /* Simple arguments handling
     *
     * TODO: use something better later on
     */

    if (args.length < 2) {
      println(help)
      return ()
    }

    val ontologyPath = new File(args(0))
    val queryPath = new File(args(1))

    if (!ontologyPath.isFile || !queryPath.isFile) {
      println("The provided arguments are not regular files.\n\n")
      println(help)
      return ()
    }

    /* Create RSA object from generic OWLOntology
     *
     * TODO: It might be required to check if the ontology in input is
     * Horn-ALCHOIQ. At the moment we are assuming this is always the
     * case.
     */
    val rsa = new RSA(ontologyPath)
    RSA.isRSA(rsa.getOntology)

    /* Build canonical model */
    //val tboxCanon = rsa.canonicalModel()

    /* Load query */
    //val query = ...

    /* Compute the filtering program from the given query */
    //val tboxFilter = rsa.filteringProgram(query)

    /* ... */

    /* DEBUG ONLY */
    println("Ok!")
  }
}

/* Notes:
 *
 * To establish a connection with a local RDFox instance, do the
 * following:
 *
 * ```
 * val serverConnection : ServerConnection = ConnectionFactory.newServerConnection("rdfox:local", "", "")
 * serverConnection.createDataStore("test","seq",new HashMap())
 * val dataStoreConnection : DataStoreConnection = serverConnection.newDataStoreConnection("test")
 * dataStoreConnection.importData(
 *    UpdateType.ADDITION,
 *    Prefixes.s_emptyPrefixes,
 *    new File("./path/to/file")
 * )
 * ```
 */
