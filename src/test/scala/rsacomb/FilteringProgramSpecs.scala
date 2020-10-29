package rsacomb

import java.io.File
import org.scalatest.LoneElement
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import tech.oxfordsemantic.jrdfox.logic.Variable
import tech.oxfordsemantic.jrdfox.Prefixes

import rsacomb.RDFoxUtil._

object FilteringProgramSpec {

  val prefixes = new Prefixes()
  prefixes.declarePrefix(
    ":",
    "http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#"
  )
  prefixes.declarePrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  prefixes.declarePrefix("rdfs:", "http://www.w3.org/2000/01/rdf-schema#")
  prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")

  val query0 = parseQuery("""
    SELECT  ?subj
    WHERE {
      ?subj  ?pred  ?obj
    }
  """, prefixes)

  val query1 = parseQuery("""
    SELECT  *
    WHERE {
      ?w  a  :Wellbore
    }
  """, prefixes)

  val query2 = parseQuery(
    """
    SELECT  *
    WHERE {
      ?w    a                     :Wellbore ;
            :wellboreDocument     ?doc .
      ?doc  :hasURL               ?document_hyperlink
    }
  """,
    prefixes
  )

  val query3 = parseQuery(
    """
    SELECT  ?w ?doc ?document_hyperlink
    WHERE {
      ?w    a                     :Wellbore ;
            :wellboreDocument     ?doc .
      ?doc  :hasURL               ?document_hyperlink
    }
  """,
    prefixes
  )

  val query4 = parseQuery(
    """
    SELECT  ?w ?doc ?document_hyperlink
    WHERE {
      ?w    a                     :Wellbore ;
            :wellboreDocument     ?doc .
      ?doc  :hasURL               ?document_hyperlink
    }
  """,
    prefixes
  )

  val query5 = parseQuery(
    """
    SELECT  ?wellbore ?unit_name ?discovery
    WHERE {
      ?w       a                     :Wellbore ;
               :name                 ?wellbore ;
               :hasWellboreInterval  ?c_int ;
               :hasWellboreInterval  ?f_int .
      ?c_int   :hasUnit              ?c_unit .
      ?c_unit  :name                 ?unit_name .
      ?f_int   a                     :FluidZone ;
               :name                 ?discovery ;
               :overlapsWellboreInterval  ?c_int
    }
  """,
    prefixes
  )

  val query6 = parseQuery(
    """
    SELECT DISTINCT ?wellbore ?content
    WHERE {
      ?w    a                     :Wellbore ;
            :name                 ?wellbore ;
            :hasWellboreInterval  ?int .
      ?int  a                     :FluidZone ;
            :fluidZoneContent     ?content
    }
  """,
    prefixes
  )

  val query7 = parseQuery(
    """
    SELECT  ?wName ?sample ?porosity ?top_depth_md ?bot_depth_md
    WHERE {
      ?w        a                     :Wellbore ;
                :name                 ?wName ;
                :hasWellboreInterval  ?z .
      ?z        :hasUnit              ?u .
      ?u        :name                 ?strat_unit_name .
      ?wellbore  :hasWellboreInterval  ?cored_int .
      ?c        :extractedFrom        ?cored_int ;
                :hasCoreSample        ?sample .
      ?sample   :hasDepth  ?sample_depth .
      ?sample_depth
                :inWellboreInterval   ?z .
      ?sample   :hasPorosity  ?p .
      ?p        :valueInStandardUnit  ?porosity .
      ?z        :hasTopDepth  ?top .
      ?top      a                     :MeasuredDepth ;
                :valueInStandardUnit  ?top_depth_md .
      ?z        :hasBottomDepth  ?bot .
      ?bot      a                     :MeasuredDepth ;
                :valueInStandardUnit  ?bot_depth_md
   }
  """,
    prefixes
  )

}

class FilteringProgramSpec
    extends AnyFlatSpec
    with Matchers
    with LoneElement
    with Inspectors {

  import FilteringProgramSpec._

  query0.toString() should "have distinct answer and bounded variables" in {
    val program = new FilteringProgram(query0, List())
    forAll(program.answer) { v => program.bounded should not contain v }
    forAll(program.bounded) { v => program.answer should not contain v }
  }

  it should "have {?obj, ?pred} as bounded variables" in {
    val pred = Variable.create("obj")
    val obj = Variable.create("pred")
    val program = new FilteringProgram(query0, List())
    program.bounded should contain theSameElementsAs List(pred, obj)
  }

  query1.toString() should "have distinct answer and bounded variables" in {
    val program = new FilteringProgram(query1, List())
    forAll(program.answer) { v => program.bounded should not contain v }
    forAll(program.bounded) { v => program.answer should not contain v }
  }

  it should "have no bounded variable" in {
    val program = new FilteringProgram(query1, List())
    program.bounded shouldBe empty
  }

  query2.toString() should "have distinct answer and bounded variables" in {
    val program = new FilteringProgram(query2, List())
    forAll(program.answer) { v => program.bounded should not contain v }
    forAll(program.bounded) { v => program.answer should not contain v }
  }
}
