package rsacomb

import java.io.File
import java.util.{ArrayList => JList}
import org.scalatest.LoneElement
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import tech.oxfordsemantic.jrdfox.logic.{Variable, Atom, IRI}
import tech.oxfordsemantic.jrdfox.logic.{Query, QueryType}
import tech.oxfordsemantic.jrdfox.logic.LogicFormat
import tech.oxfordsemantic.jrdfox.Prefixes

import scala.collection.JavaConverters._

import rsacomb.RDFoxUtil._
import tech.oxfordsemantic.jrdfox.logic.Conjunction

object FilteringProgramSpec {

  val prefixes = new Prefixes()
  prefixes.declarePrefix(
    ":",
    "http://slegger.gitlab.io/slegge-obda/ontology/subsurface-exploration#"
  )
  prefixes.declarePrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  prefixes.declarePrefix("rdfs:", "http://www.w3.org/2000/01/rdf-schema#")
  prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")

  // DEBUG: Quick helper functions
  def v(v: String): Variable = Variable.create(v)
  def c(c: String): IRI = IRI.create(":" + c)

  // QUERY 0

  // val query0 = parseQuery("""
  //   SELECT  ?subj
  //   WHERE {
  //     ?subj  ?pred  ?obj
  //   }
  // """, prefixes)

  val query0 = Query.create(
    QueryType.SELECT,
    false,
    List(v("subj")).asJava,
    Atom.rdf(v("subj"), v("pred"), v("obj"))
  )

  // QUERY 1

  // val query1 = parseQuery("""
  //   SELECT  *
  //   WHERE {
  //     ?w  a  :Wellbore
  //   }
  // """, prefixes)

  val query1 = Query.create(
    QueryType.SELECT,
    false,
    List(v("w")).asJava,
    Atom.rdf(v("w"), IRI.RDF_TYPE, c("Wellbore"))
  )

  // QUERY 2

  // val query2 = parseQuery(
  //   """
  //   SELECT  *
  //   WHERE {
  //     ?w    a                     :Wellbore ;
  //           :wellboreDocument     ?doc .
  //     ?doc  :hasURL               ?document_hyperlink
  //   }
  // """,
  //   prefixes
  // )

  val query2 = Query.create(
    QueryType.SELECT,
    false,
    List(v("w"), v("doc"), v("doc"), v("document_hyperlink")).asJava,
    Conjunction.create(
      Atom.rdf(v("w"), IRI.RDF_TYPE, c("Wellbore")),
      Atom.rdf(v("w"), c("wellboreDocument"), v("doc")),
      Atom.rdf(v("doc"), c("hasURL"), v("document_hyperlink"))
    )
  )

  // QUERY 3

  // val query3 = parseQuery(
  //   """
  //   SELECT  ?w ?doc ?document_hyperlink
  //   WHERE {
  //     ?w    a                     :Wellbore ;
  //           :wellboreDocument     ?doc .
  //     ?doc  :hasURL               ?document_hyperlink
  //   }
  // """,
  //   prefixes
  // )

  val query3 = Query.create(
    QueryType.SELECT,
    false,
    List(v("w"), v("doc"), v("document_hyperlink")).asJava,
    Conjunction.create(
      Atom.rdf(v("w"), IRI.RDF_TYPE, c("Wellbore")),
      Atom.rdf(v("w"), c("wellboreDocument"), v("doc")),
      Atom.rdf(v("doc"), c("hasURL"), v("document_hyperlink"))
    )
  )

  // QUERY 4

  // val query4 = parseQuery(
  //   """
  //   SELECT  ?w ?doc ?document_hyperlink
  //   WHERE {
  //     ?w    a                     :Wellbore ;
  //           :wellboreDocument     ?doc .
  //     ?doc  :hasURL               ?document_hyperlink
  //   }
  // """,
  //   prefixes
  // )

  val query4 = Query.create(
    QueryType.SELECT,
    false,
    List(v("w"), v("doc"), v("document_hyperlink")).asJava,
    Conjunction.create(
      Atom.rdf(v("w"), IRI.RDF_TYPE, c("Wellbore")),
      Atom.rdf(v("w"), c("wellboreDocument"), v("doc")),
      Atom.rdf(v("doc"), c("hasURL"), v("document_hyperlink"))
    )
  )

  // QUERY 5

  // val query5 = parseQuery(
  //   """
  //   SELECT  ?wellbore ?unit_name ?discovery
  //   WHERE {
  //     ?w       a                     :Wellbore ;
  //              :name                 ?wellbore ;
  //              :hasWellboreInterval  ?c_int ;
  //              :hasWellboreInterval  ?f_int .
  //     ?c_int   :hasUnit              ?c_unit .
  //     ?c_unit  :name                 ?unit_name .
  //     ?f_int   a                     :FluidZone ;
  //              :name                 ?discovery ;
  //              :overlapsWellboreInterval  ?c_int
  //   }
  // """,
  //   prefixes
  // )

  val query5 = Query.create(
    QueryType.SELECT,
    false,
    List(v("wellbore"), v("unit_name"), v("discovery")).asJava,
    Conjunction.create(
      Atom.rdf(v("w"), IRI.RDF_TYPE, c("Wellbore")),
      Atom.rdf(v("w"), c("name"), v("wellbore")),
      Atom.rdf(v("w"), c("hasWellboreInterval"), v("c_int")),
      Atom.rdf(v("w"), c("hasWellboreInterval"), v("f_int")),
      Atom.rdf(v("c_int"), c("hasUnit"), v("c_unit")),
      Atom.rdf(v("c_unit"), c("name"), v("unit_name")),
      Atom.rdf(v("f_int"), IRI.RDF_TYPE, c("FluidZone")),
      Atom.rdf(v("f_int"), c("name"), v("discovery")),
      Atom.rdf(v("f_int"), c("overlapsWellboreInterval"), v("c_int"))
    )
  )

  // QUERY 6

  // val query6 = parseQuery(
  //   """
  //   SELECT DISTINCT ?wellbore ?content
  //   WHERE {
  //     ?w    a                     :Wellbore ;
  //           :name                 ?wellbore ;
  //           :hasWellboreInterval  ?int .
  //     ?int  a                     :FluidZone ;
  //           :fluidZoneContent     ?content
  //   }
  // """,
  //   prefixes
  // )

  val query6 = Query.create(
    QueryType.SELECT,
    true,
    List(v("wellbore"), v("content")).asJava,
    Conjunction.create(
      Atom.rdf(v("w"), IRI.RDF_TYPE, c("Wellbore")),
      Atom.rdf(v("w"), c("name"), v("wellbore")),
      Atom.rdf(v("w"), c("hasWellboreInterval"), v("int")),
      Atom.rdf(v("int"), IRI.RDF_TYPE, c("FluidZone")),
      Atom.rdf(v("int"), c("fluidZoneContent"), v("content"))
    )
  )

  // QUERY 7

  // val query7 = parseQuery(
  //   """
  //   SELECT  ?wName ?sample ?porosity ?top_depth_md ?bot_depth_md
  //   WHERE {
  //     ?w        a                     :Wellbore ;
  //               :name                 ?wName ;
  //               :hasWellboreInterval  ?z .
  //     ?z        :hasUnit              ?u .
  //     ?u        :name                 ?strat_unit_name .
  //     ?wellbore  :hasWellboreInterval  ?cored_int .
  //     ?c        :extractedFrom        ?cored_int ;
  //               :hasCoreSample        ?sample .
  //     ?sample   :hasDepth  ?sample_depth .
  //     ?sample_depth
  //               :inWellboreInterval   ?z .
  //     ?sample   :hasPorosity  ?p .
  //     ?p        :valueInStandardUnit  ?porosity .
  //     ?z        :hasTopDepth  ?top .
  //     ?top      a                     :MeasuredDepth ;
  //               :valueInStandardUnit  ?top_depth_md .
  //     ?z        :hasBottomDepth  ?bot .
  //     ?bot      a                     :MeasuredDepth ;
  //               :valueInStandardUnit  ?bot_depth_md
  //  }
  // """,
  //   prefixes
  // )

  val query7 = Query.create(
    QueryType.SELECT,
    false,
    List(
      v("wName"),
      v("sample"),
      v("porosity"),
      v("top_depth_md"),
      v("bot_depth_md")
    ).asJava,
    Conjunction.create(
      Atom.rdf(v("w"), IRI.RDF_TYPE, c("Wellbore")),
      Atom.rdf(v("w"), c("name"), v("wName")),
      Atom.rdf(v("w"), c("hasWellboreInterval"), v("z")),
      Atom.rdf(v("z"), c("hasUnit"), v("u")),
      Atom.rdf(v("u"), c("name"), v("strat_unit_name")),
      Atom.rdf(v("wellbore"), c("hasWellboreInterval"), v("cored_int")),
      Atom.rdf(v("c"), c("extractedFrom"), v("cored_int")),
      Atom.rdf(v("c"), c("hasCoreSample"), v("sample")),
      Atom.rdf(v("sample"), c("hasDepth"), v("sample_depth")),
      Atom.rdf(v("sample_depth"), c("inWellboreInterval"), v("z")),
      Atom.rdf(v("sample"), c("hasPorosity"), v("p")),
      Atom.rdf(v("p"), c("valueInStandardUnit"), v("porosity")),
      Atom.rdf(v("z"), c("hasTopDepth"), v("top")),
      Atom.rdf(v("top"), IRI.RDF_TYPE, c("MeasuredDepth")),
      Atom.rdf(v("top"), c("valueInStandardUnit"), v("top_depth_md")),
      Atom.rdf(v("z"), c("hasBottomDepth"), v("bot")),
      Atom.rdf(v("bot"), IRI.RDF_TYPE, c("MeasuredDepth")),
      Atom.rdf(v("bot"), c("valueInStandardUnit"), v("bot_depth_md"))
    )
  )

  val queries =
    List(query0, query1, query2, query3, query4, query5, query6, query7)

}

class FilteringProgramSpec
    extends AnyFlatSpec
    with Matchers
    with LoneElement
    with Inspectors {

  import FilteringProgramSpec._

  "Queries" should "have distinct answer and bounded variables" in {
    for (query <- queries) {
      val program = new FilteringProgram(query, List())
      forAll(program.answer) { v => program.bounded should not contain v }
      forAll(program.bounded) { v => program.answer should not contain v }
    }
  }

  query0.toString() should "have {?obj, ?pred} as bounded variables" in {
    val pred = Variable.create("obj")
    val obj = Variable.create("pred")
    val program = new FilteringProgram(query0, List())
    program.bounded should contain theSameElementsAs List(pred, obj)
  }

  query1.toString() should "have no bounded variable" in {
    val program = new FilteringProgram(query1, List())
    program.bounded shouldBe empty
  }

  query2.toString() should "have no bounded variable" in {
    val program = new FilteringProgram(query2, List())
    program.bounded shouldBe empty
  }
}
