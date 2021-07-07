package org.enso.docs.generator

import org.enso.docs.generator.DocParserWrapper.runOnPureDoc
import org.enso.syntax.text.AST
import org.enso.syntax.text.docparser.DocParserHTMLGenerator
import scalatags.Text.{all => HTML}
import HTML._

/** Defines methods for generating and glue-ing together doc content.
  */
object DocsGenerator {

  /** Generates list of HTML docs from given doc comments in AST.
    */
  def generateFromAst(ast: List[AST.Comment]): List[String] = {
    generate(ast.map(_.show()))
  }

  /** Generates list of HTML docs from given doc comments.
    */
  def generate(comments: List[String]): List[String] = {
    comments.map(runOnPureDoc)
  }

  /** Connects HTML documentation with it's AST element.
    */
  def connectHtmlToAst(html: String, ast: AST): String = {
    val astHTML = DocParserHTMLGenerator.createHTMLFromAST(ast)
    val astName = HTML.div(astHTML.header)
    astHTML.body match {
      case Some(body) =>
        HTML.div(HTML.`class` := "main ml-20")(astName, html, body).render
      case None => HTML.div(astName, html).render
    }
  }

  /** Glues together many docs with AST elements in proper order to create one
    * documentation page.
    */
  def glueGeneratedContent(docs: List[String], astList: List[AST]): String = {
    docs.zip(astList).map(e => connectHtmlToAst(e._1, e._2)).mkString
  }
}
