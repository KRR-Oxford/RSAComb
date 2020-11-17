package rsacomb

/* Java imports */
import java.util.Map

import tech.oxfordsemantic.jrdfox.formats.SPARQLParser
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.expression.{Variable, IRI}
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.{
  OWLAxiom,
  OWLClass,
  OWLObjectPropertyExpression
}

// Debug only
import scala.collection.JavaConverters._

object RSA extends RSAAxiom {

  val Prefixes: Prefixes = new Prefixes()
  Prefixes.declarePrefix(":", "http://example.com/rsa_example.owl#")
  Prefixes.declarePrefix("rsa:", "http://127.0.0.1/")
  Prefixes.declarePrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  Prefixes.declarePrefix("rdfs:", "http://www.w3.org/2000/01/rdf-schema#")
  Prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")

  val EquivTo: IRI = this.rsa("EquivTo")
  val Named: IRI = this.rsa("NAMED")

  // Counter used to implement a simple fresh variable generator
  private var counter = -1;

  def getFreshVariable(): Variable = {
    counter += 1
    Variable.create(f"I$counter%03d")
  }

  def base(name: Any): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get(":").getIRI
        + name.toString
    )

  def rsa(name: Any): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get("rsa:").getIRI
        + name.toString
    )

  def hashed(
      cls1: OWLClass,
      prop: OWLObjectPropertyExpression,
      cls2: OWLClass
  ): String =
    (cls1, prop, cls2).hashCode.toString

  def hashed(axiom: OWLAxiom): String = {
    val (cls1, prop, cls2) = axiom.toTriple.get
    this.hashed(cls1, prop, cls2)
  }

} // object RSA
