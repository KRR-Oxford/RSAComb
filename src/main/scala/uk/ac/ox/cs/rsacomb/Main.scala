/*
 * Copyright 2020-2022 KRR Oxford
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

import approximation.{Lowerbound,Upperbound}
import converter.Normalizer
import ontology.{Ontology,RSAOntology}
import util.{Logger, RDFoxUtil, RSA}

object RSAComb extends App {
  implicit val config = RSAConfig parse args.toList
  RSAConfig describe config

  /* Configure logger */
  if (config.contains('logger))
    Logger.level = config('logger).get[Logger.Level]
  if (config.contains('answers))
    Logger.answers = config('answers).get[os.Path]

  /* Load original ontology and normalize it */
  val ontopath = config('ontology).get[os.Path]
  val datapath = config('data).get[List[os.Path]]
  val ontology = Ontology(ontopath, datapath).normalize(new Normalizer)

  /* Approximate the ontology if necessary */
  val toRSA = config('approximation).get[Symbol] match {
    case 'lowerbound => new Lowerbound
    case 'upperbound => new Upperbound
  }
  val rsa = ontology approximate toRSA

  if (config contains 'queries) {
    val queries =
      RDFoxUtil.loadQueriesFromFiles(
        config('queries).get[List[os.Path]],
        RSA.Prefixes
      )

    /* Perform query answering */
    val answers = rsa ask queries

    /* Perform logging */
    Logger write answers
    Logger.generateSimulationScripts(datapath, queries)
  }
}
