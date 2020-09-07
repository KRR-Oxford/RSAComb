package rsacomb

/* Java imports */
import java.io.File
import java.util.Map

import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.IRI
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology
import rsacomb.RSAOntology

// Debug only
import tech.oxfordsemantic.jrdfox.logic.{
  Formula,
  Atom,
  Variable,
  Query,
  QueryType,
  Conjunction
}
import scala.collection.JavaConverters._

object RSA extends RSAOntology {

  val Prefixes = new Prefixes()
  Prefixes.declarePrefix(":", "http://example.com/rsa_example.owl#")
  Prefixes.declarePrefix("internal:", "http://127.0.0.1/")
  Prefixes.declarePrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  Prefixes.declarePrefix("rdfs:", "http://www.w3.org/2000/01/rdf-schema#")
  Prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")

  val varX = Variable.create("X")
  val varY = Variable.create("Y")
  val varZ = Variable.create("Z")
  val testAnswerVars = List[Variable](varX, varY, varZ).asJava;
  val testFormula: Formula =
    Conjunction.create(
      Atom.rdf(varX, IRI.TOP_OBJECT_PROPERTY, varY),
      Atom.rdf(varY, IRI.TOP_OBJECT_PROPERTY, varZ)
    )
  val test_query =
    Query.create(QueryType.SELECT, false, testAnswerVars, testFormula)

  def internal(name: String): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get("internal:").getIRI + name
    )

  // TODO: move this somewhere else... maybe an OntoUtils class or something.
  def loadOntology(onto: File): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()
    manager.loadOntologyFromOntologyDocument(onto)
  }

} // object RSA
