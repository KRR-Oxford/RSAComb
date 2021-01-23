package uk.ac.ox.cs.rsacomb.implicits

import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.expression.{
  BlankNode,
  IRI => RDFoxIRI,
  Literal
}
import org.semanticweb.owlapi.model.{
  IRI => OWLIRI,
  NodeID,
  OWLLiteral,
  OWLDatatype
}
import org.semanticweb.owlapi.vocab.OWL2Datatype

object RDFox {

  implicit def rdfoxToOwlapiIri(iri: RDFoxIRI): OWLIRI =
    OWLIRI.create(iri.getIRI)

  implicit def owlapiToRdfoxIri(iri: OWLIRI): RDFoxIRI =
    RDFoxIRI.create(iri.getIRIString())

  implicit def nodeIdToBlankNode(node: NodeID): BlankNode =
    BlankNode.create(node.getID)

  implicit def blankNodeToNodeId(node: BlankNode): NodeID =
    NodeID.getNodeID(node.getID)

  implicit def stringToRdfoxIri(iri: String): RDFoxIRI =
    RDFoxIRI.create(iri)

  /** Converst an OWLAPI datatype into an RDFox datatype.
    *
    * The builtin datatypes defined by the two systems do not match
    * perfectly. In particular these entities cannot be directly
    * translated.
    *
    * From the OWLAPI (mapped to `INVALID_DATATYPE`):
    * - OWL_RATIONAL
    * - OWL_REAL
    * - RDF_LANG_STRING
    * - RDF_XML_LITERAL
    * - XSD_BASE_64_BINARY
    * - XSD_HEX_BINARY
    * - XSD_LANGUAGE
    * - XSD_NAME
    * - XSD_NCNAME
    * - XSD_NMTOKEN
    * - XSD_NORMALIZED_STRING
    * - XSD_TOKEN
    *
    * From RDFox:
    * - BLANK_NODE
    * - IRI_REFERENCE
    * - XSD_DATE
    * - XSD_DAY_TIME_DURATION
    * - XSD_DURATION
    * - XSD_G_DAY
    * - XSD_G_MONTH
    * - XSD_G_MONTH_DAY
    * - XSD_G_YEAR
    * - XSD_G_YEAR_MONTH
    * - XSD_TIME
    * - XSD_YEAR_MONTH_DURATION
    */
  implicit def owlapiToRdfoxDatatype(datatype: OWLDatatype): Datatype =
    if (datatype.isBuiltIn) {
      datatype.getBuiltInDatatype match {
        case OWL2Datatype.RDF_PLAIN_LITERAL    => Datatype.RDF_PLAIN_LITERAL
        case OWL2Datatype.RDFS_LITERAL         => Datatype.RDFS_LITERAL
        case OWL2Datatype.XSD_ANY_URI          => Datatype.XSD_ANY_URI
        case OWL2Datatype.XSD_BOOLEAN          => Datatype.XSD_BOOLEAN
        case OWL2Datatype.XSD_BYTE             => Datatype.XSD_BYTE
        case OWL2Datatype.XSD_DATE_TIME        => Datatype.XSD_DATE_TIME
        case OWL2Datatype.XSD_DATE_TIME_STAMP  => Datatype.XSD_DATE_TIME_STAMP
        case OWL2Datatype.XSD_DECIMAL          => Datatype.XSD_DECIMAL
        case OWL2Datatype.XSD_DOUBLE           => Datatype.XSD_DOUBLE
        case OWL2Datatype.XSD_FLOAT            => Datatype.XSD_FLOAT
        case OWL2Datatype.XSD_INT              => Datatype.XSD_INT
        case OWL2Datatype.XSD_INTEGER          => Datatype.XSD_INTEGER
        case OWL2Datatype.XSD_LONG             => Datatype.XSD_LONG
        case OWL2Datatype.XSD_NEGATIVE_INTEGER => Datatype.XSD_NEGATIVE_INTEGER
        case OWL2Datatype.XSD_NON_NEGATIVE_INTEGER =>
          Datatype.XSD_NON_NEGATIVE_INTEGER
        case OWL2Datatype.XSD_NON_POSITIVE_INTEGER =>
          Datatype.XSD_NON_POSITIVE_INTEGER
        case OWL2Datatype.XSD_POSITIVE_INTEGER => Datatype.XSD_POSITIVE_INTEGER
        case OWL2Datatype.XSD_SHORT            => Datatype.XSD_SHORT
        case OWL2Datatype.XSD_STRING           => Datatype.XSD_STRING
        case OWL2Datatype.XSD_UNSIGNED_BYTE    => Datatype.XSD_UNSIGNED_BYTE
        case OWL2Datatype.XSD_UNSIGNED_INT     => Datatype.XSD_UNSIGNED_INT
        case OWL2Datatype.XSD_UNSIGNED_LONG    => Datatype.XSD_UNSIGNED_LONG
        case OWL2Datatype.XSD_UNSIGNED_SHORT   => Datatype.XSD_UNSIGNED_SHORT
        case _                                 => Datatype.INVALID_DATATYPE
      }
    } else {
      throw new RuntimeException(
        s"Composite datatypes are not allowed."
      )
    }

  implicit def owlapiToRdfoxLiteral(lit: OWLLiteral): Literal =
    Literal.create(lit.getLiteral, lit.getDatatype)

}
