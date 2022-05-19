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

package uk.ac.ox.cs.rsacomb.util

/* Java imports */
import java.util.Map

import tech.oxfordsemantic.jrdfox.formats.SPARQLParser
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Negation,
  Rule,
  TupleTableAtom,
  TupleTableName,
}
import tech.oxfordsemantic.jrdfox.logic.expression.{Term, Variable, IRI}
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.{
  OWLAxiom,
  OWLClass,
  OWLObjectPropertyExpression
}

import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.suffix.RSASuffix

// Debug only
import scala.collection.JavaConverters._

object RSA {

  /** Simplify conversion between Java and Scala `List`s */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Experimental DLS for RDFox */
  import RDFoxDSL._

  /** Set of default prefixes to be included in all datastore operations */
  val Prefixes: Prefixes = new Prefixes()
  Prefixes.declarePrefix("xml:", "http://www.w3.org/XML/1998/namespace")
  Prefixes.declarePrefix("xsd:", "http://www.w3.org/2001/XMLSchema#")
  Prefixes.declarePrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  Prefixes.declarePrefix("rdfs:", "http://www.w3.org/2000/01/rdf-schema#")
  Prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")
  Prefixes.declarePrefix("rdfox:", "http://oxfordsemantic.tech/RDFox#")
  Prefixes.declarePrefix("rsacomb:", "http://www.cs.ox.ac.uk/isg/RSAComb#")

  /** Creates a `rsacomb:<name>` IRI */
  def apply(name: Any): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get("rsacomb:").getIRI + name.toString
    )

  /** Helper IRIs */
  val ANS = RSA("Ans")
  val AQ = RSA("AQ")
  val CONGRUENT = RSA("congruent")
  val FK = RSA("FK")
  val ID = RSA("ID")
  val IN = RSA("In")
  val NAMED = RSA("Named")
  val NI = RSA("NI")
  val QM = RSA("QM")
  val SP = RSA("SP")
  val TQ = RSA("TQ")
  val E = RSA("E")
  val U = RSA("U")

  def Named(tt: TupleTableName)(x: Term): TupleTableAtom =
    TupleTableAtom.create(tt, x, IRI.RDF_TYPE, RSA.NAMED)
  def Congruent(tt: TupleTableName)(x: Term, y: Term): TupleTableAtom =
    TupleTableAtom.create(tt, x, RSA.CONGRUENT, y)
  def Skolem(skolem: Term, terms: List[Term]): TupleTableAtom =
    TupleTableAtom.create(TupleTableName.SKOLEM, terms :+ skolem)
  def E(tt: TupleTableName)(x: Term, y: Term): TupleTableAtom =
    TupleTableAtom.create(tt, x, RSA.E, y)
  def PE(tt: TupleTableName)(x: Term, y: Term) =
    TupleTableAtom.create(tt, x, RSA("PE"), y)
  def U(tt: TupleTableName)(x: Term): TupleTableAtom =
    TupleTableAtom.create(tt, x, IRI.RDF_TYPE, RSA.U)

  // def In(t: Term)(implicit set: Term) =
  //   TupleTableAtom.rdf(t, RSA("In"), set)
  // def NotIn(t: Term)(implicit set: Term) = Negation.create(In(t)(set))
  // private def atom(name: IRI, vars: List[Term]): TupleTableAtom =
  //   TupleTableAtom.create(TupleTableName.create(name.getIRI), vars: _*)

  def RBoxReasoning(graph: IRI): List[Rule] = {
    val tt = TupleTableName.create(graph.getIRI)
    val subPropertyOf = IRI.create(Prefixes.getPrefixIRIsByPrefixName.get("rdfs:").getIRI + "subPropertyOf")
    val subPropertyOfTrans = RSA("subPropertyOfTrans")
    val inverseOf = IRI.create(Prefixes.getPrefixIRIsByPrefixName.get("owl:").getIRI + "inverseOf")
    List(
      Rule.create(
        TupleTableAtom.create(tt, v"X", subPropertyOfTrans, v"Y"),
        TupleTableAtom.create(tt, v"X", subPropertyOf, v"Y"),
      ),
      Rule.create(
        TupleTableAtom.create(tt, v"X", subPropertyOfTrans, v"Z"),
        TupleTableAtom.create(tt, v"X", subPropertyOfTrans, v"Y"),
        TupleTableAtom.create(tt, v"Y", subPropertyOfTrans, v"Z")
      ),
      Rule.create(
        TupleTableAtom.create(tt, v"Yi", subPropertyOf, v"Xi"),
        TupleTableAtom.create(tt, v"X", subPropertyOf, v"Y"),
        TupleTableAtom.create(tt, v"Xi", inverseOf, v"X"),
        TupleTableAtom.create(tt, v"Yi", inverseOf, v"Y"),
      ),
      Rule.create(
        TupleTableAtom.create(tt, v"X", inverseOf, v"Y"),
        TupleTableAtom.create(tt, v"Y", inverseOf, v"X"),
      )
    )
  }

}

/** Some common ontology prefixes:
  *
  * @prefix dc: <http://purl.org/dc/elements/1.1/> .
  * @prefix ex: <http://semantics.id/ns/example#> .
  * @prefix fn: <http://www.w3.org/2005/xpath-functions> .
  * @prefix ms: <http://www.novartis.com/metastore#> .
  * @prefix vb: <http://eresources.nlb.gov.sg/ID/NLBDM/vocab/> .
  * @prefix xs: <http://www.w3.org/2001/XMLSchema> .
  * @prefix NBN: <http://urn.fi/#> .
  * @prefix XML: <http://www.w3.org/XML/1998/> .
  * @prefix bot: <https://w3id.org/bot#> .
  * @prefix cbx: <http://cybox.mitre.org/cybox_v1#> .
  * @prefix dct: <http://purl.org/dc/terms/> .
  * @prefix esv: <http://eresources.nlb.gov.sg/ID/NLBDM/vocab-esv/> .
  * @prefix gss: <http://www.w3.org/2001/11/IsaViz/graphstylesheets#> .
  * @prefix ioc: <http://w3id.org/ioc#> .
  * @prefix j.0: <http://www.eurovision.com#> .
  * @prefix mlo: <http://www.a2rd.net.br/mlo#> .
  * @prefix ns1: <http://eresources.nlb.gov.sg/ID/NLBDM/marcext/> .
  * @prefix ns2: <http://bibfra.me/vocab/marc/> .
  * @prefix ns3: <http://eresources.nlb.gov.sg/ID/NLBDM/vocab-ccv/> .
  * @prefix ns4: <http://bio2rdf.org/bio2rdf_vocabulary:> .
  * @prefix obo: <http://purl.obolibrary.org/obo/> .
  * @prefix owl: <http://www.w3.org/2002/07/owl#> .
  * @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  * @prefix tmp: <http://www.semanticweb.org/maria/ontologies/2021/9/running_example#> .
  * @prefix www: <http://www.movieontology.org/2009/11/09/> .
  * @prefix xml: <http://www.w3.org/XML/1998/namespace> .
  * @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
  * @prefix xsp: <http://www.owl-ontologies.com/2005/08/07/xsp.owl#> .
  * @prefix bibo: <http://purl.org/ontology/bibo/> .
  * @prefix daml: <http://www.daml.org/2001/03/daml+oil#> .
  * @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  * @prefix lego: <http://spatialai.org/lego/core/v1#> .
  * @prefix mlo1: <http://www.a2rd.net.br/mlo#mlo:> .
  * @prefix mlo2: <http://www.semanticweb.org/user/ontologies/2020/0/ml-ontology#mlo:> .
  * @prefix obda: <https://w3id.org/obda/vocabulary#> .
  * @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
  * @prefix skos: <http://www.w3.org/2004/02/skos/core#> .
  * @prefix swrl: <http://www.w3.org/2003/11/swrl#> .
  * @prefix vann: <http://purl.org/vocab/vann/> .
  * @prefix void: <http://rdfs.org/ns/void#> .
  * @prefix admin: <http://webns.net/mvcb/> .
  * @prefix nlbdm: <http://eresources.nlb.gov.sg/ID/NLBDM/vocab/> .
  * @prefix pizza: <http://www.co-ode.org/ontologies/pizza/pizza.owl#> .
  * @prefix pstcn: <http://burningbird.net/postcon/elements/1.0/> .
  * @prefix swrla: <http://swrl.stanford.edu/ontologies/3.3/swrla.owl#> .
  * @prefix swrlb: <http://www.w3.org/2003/11/swrlb#> .
  * @prefix terms: <http://purl.org/dc/terms/> .
  * @prefix vcard: <http://www.w3.org/2006/vcard/ns#> .
  * @prefix PLCore: <http://www.perpetuallabs.io/ontologies/2021/9/PLCore#> .
  * @prefix Server: <urn:absolute:Server#> .
  * @prefix TypeQL: <http://www.perpetuallabs.io/ontologies/2021/9/TypeQL#> .
  * @prefix schema: <http://schema.org/> .
  * @prefix travel: <http://www.owl-ontologies.com/travel.owl#> .
  * @prefix current: <http://erlangen-crm.org/current/> .
  * @prefix dcterms: <http://purl.org/dc/terms/> .
  * @prefix ebucore: <http://www.ebu.ch/metadata/ontologies/ebucore/ebucore#> .
  * @prefix example: <http://www.ics.forth.gr/example#> .
  * @prefix ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#> .
  * @prefix opla-sd: <http://ontologydesignpatterns.org/opla-sd#> .
  * @prefix owl2xml: <http://www.w3.org/2006/12/owl2-xml#> .
  * @prefix protege: <http://protege.stanford.edu/plugins/owl/protege#> .
  * @prefix mappings: <http://www.cs.ox.ac.uk/isg/projects/LogMap/mappings.owl#> .
  * @prefix oboInOwl: <http://www.geneontology.org/formats/oboInOwl#> .
  * @prefix otherOnt: <http://example.org/otherOntologies/families/> .
  * @prefix EurosData: <http://www.EurosFile.com/EurosData#> .
  * @prefix geosparql: <http://www.opengis.net/ont/geosparql#> .
  * @prefix restaurant: <http://www.univrouen.fr/ontologies/restaurant#> .
  * @prefix ml-ontology: <http://www.semanticweb.org/user/ontologies/2020/0/ml-ontology#> .
  */
