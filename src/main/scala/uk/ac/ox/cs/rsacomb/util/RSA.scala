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
  TupleTableAtom,
  TupleTableName,
  Negation
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

  /** Set of default prefixes to be included in all datastore operations */
  val Prefixes: Prefixes = new Prefixes()
  Prefixes.declarePrefix("rsacomb:", "http://www.cs.ox.ac.uk/isg/RSAComb#")
  Prefixes.declarePrefix("rdfox:", "http://oxfordsemantic.tech/RDFox#")
  Prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")

  /** Creates a `rsacomb:<name>` IRI */
  def apply(name: Any): IRI =
    IRI.create(
      //Prefixes.decodeIRI("rsacomb:") + name.toString
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

  def Named(tt: TupleTableName)(x: Term): TupleTableAtom =
    TupleTableAtom.create(tt, x, IRI.RDF_TYPE, RSA.NAMED)
  def Congruent(tt: TupleTableName)(x: Term, y: Term): TupleTableAtom =
    TupleTableAtom.create(tt, x, RSA.CONGRUENT, y)
  def Skolem(skolem: Term, terms: List[Term]): TupleTableAtom =
    TupleTableAtom.create(TupleTableName.SKOLEM, terms :+ skolem)

  // def In(t: Term)(implicit set: Term) =
  //   TupleTableAtom.rdf(t, RSA("In"), set)

  // def NotIn(t: Term)(implicit set: Term) = Negation.create(In(t)(set))

  /* TODO: review after reworking the dependency graph construction */

  // private def atom(name: IRI, vars: List[Term]): TupleTableAtom =
  //   TupleTableAtom.create(TupleTableName.create(name.getIRI), vars: _*)

  def E(t1: Term, t2: Term) =
    TupleTableAtom.rdf(t1, RSA("E"), t2)

  def PE(t1: Term, t2: Term) =
    TupleTableAtom.rdf(t1, RSA("PE"), t2)

  def U(t: Term) =
    TupleTableAtom.rdf(t, IRI.RDF_TYPE, RSA("U"))

}
