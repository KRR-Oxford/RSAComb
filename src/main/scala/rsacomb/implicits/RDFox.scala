package rsacomb.implicits

import tech.oxfordsemantic.jrdfox.logic.expression.{IRI => RDFoxIRI}
import org.semanticweb.owlapi.model.{IRI => OWLIRI}

object RDFox {

  implicit def rdfoxToOwlapiIri(iri: RDFoxIRI): OWLIRI = {
    OWLIRI.create(iri.getIRI)
  }

  implicit def owlapiToRdfoxIri(iri: OWLIRI): RDFoxIRI = {
    RDFoxIRI.create(iri.getIRIString())
  }

  implicit def stringToRdfoxIri(iri: String): RDFoxIRI = {
    RDFoxIRI.create(iri)
  }

}
