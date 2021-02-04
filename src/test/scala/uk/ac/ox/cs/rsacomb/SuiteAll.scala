package uk.ac.ox.cs.rsacomb

import org.scalatest.Suites

import uk.ac.ox.cs.rsacomb.converter.{
  OWLAxiomSpec,
  OWLClassSpec,
  RDFoxConverterSpec
}
import uk.ac.ox.cs.rsacomb.filtering.NaiveFilteringProgramSpec
import uk.ac.ox.cs.rsacomb.sparql.{
  ConjunctiveQueryAnswerSpec,
  ConjunctiveQuerySpec
}

class SuiteAll
    extends Suites(
      new Ontology1_CanonicalModelSpec,
      new Ontology2_CanonicalModelSpec,
      new NaiveFilteringProgramSpec,
      new OWLAxiomSpec,
      new OWLClassSpec,
      new RDFoxConverterSpec,
      new ConjunctiveQueryAnswerSpec,
      new ConjunctiveQuerySpec
    )
