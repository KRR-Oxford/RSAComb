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

import java.io.{File, PrintWriter}
import java.nio.file.{Path, Paths, InvalidPathException}
import java.util.HashMap
import scala.collection.JavaConverters._
import tech.oxfordsemantic.jrdfox.client.UpdateType
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI, Term}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

import util.{Logger, RDFoxUtil, RSA}
import sparql.ConjunctiveQuery

import uk.ac.ox.cs.rsacomb.ontology.Ontology
import uk.ac.ox.cs.rsacomb.converter.Normalizer
import uk.ac.ox.cs.rsacomb.approximation.{Upperbound, Lowerbound}

/** Main entry point to the program */
object RSAComb extends App {

  /* Command-line options */
  val config = RSAConfig.parse(args.toList)

  /* Set logger level */
  if (config.contains('logger))
    Logger.level = config('logger).get[Logger.Level]

  /* Set answers output file */
  if (config.contains('answers))
    Logger.answers = config('answers).get[os.Path]

  /* Load original ontology and normalize it */
  val ontopath = config('ontology).get[os.Path]
  val data = config('data).get[List[os.Path]]
  val ontology = Ontology(ontopath, data).normalize(new Normalizer)

  //ontology.axioms foreach println

  /* Approximate the ontology to RSA */
  val toRSA = new Upperbound
  val rsa = ontology approximate toRSA

  if (config contains 'queries) {
    val queries =
      RDFoxUtil.loadQueriesFromFile(
        config('queries).get[os.Path]
      )

    val answers = rsa ask queries

    /* Write answers to output file */
    Logger write answers
    /* Generate simulation script */
    Logger.generateSimulationScripts(data, queries)
  }
}
