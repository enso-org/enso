package org.enso.syntax.text

import io.circe.syntax._
import org.enso.flexer.Reader
import org.enso.syntax.text.Parser.ParserError

import scala.scalajs.js.annotation._

object Parse {
  @JSExportTopLevel("parse")
  def parse(program: String, idsJson: String): String = {
    val idmap = Parser
      .idMapFromJson(idsJson)
      .left
      .map { error =>
        throw new ParserError("Could not deserialize idmap.", error)
      }
      .merge
    new Parser().run(new Reader(program), idmap).toJson().noSpacesSortKeys
  }

  @JSExportTopLevel("parse_with_metadata")
  def parse_with_metadata(program: String): String = {
    new Parser().runWithMetadata(program).asJson.noSpacesSortKeys
  }

  @JSExportTopLevel("doc_parser_generate_html_source")
  def doc_parser_generate_html_source(program: String): String = {
    val parser   = new Parser()
    val module   = parser.run(program)
    val dropMeta = parser.dropMacroMeta(module)
    val doc      = DocParserRunner.createDocs(dropMeta)
    val htmlCode = DocParserHTMLGenerator.generateHTMLForEveryDocumented(doc)
    htmlCode
  }

  @JSExportTopLevel("doc_parser_generate_html_from_doc")
  def doc_parser_generate_html_from_doc(code: String): String = {
    val doc      = DocParser.runMatched(code)
    val htmlCode = DocParserHTMLGenerator.generateHTMLPureDoc(doc)
    htmlCode
  }
}
