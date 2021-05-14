package org.enso.docs.generator

import java.io.File
import org.enso.syntax.text.{AST, DocParser, Parser}
import org.enso.syntax.text.docparser._
import scalatags.Text.{all => HTML}
import HTML._

/** The Docs Generator class. Defines useful wrappers for Doc Parser.
  */
object DocsGenerator {

  /** Generates HTML of docs from Enso program.
    */
  def run(program: String): String = {
    val parser   = new Parser()
    val module   = parser.run(program)
    val dropMeta = parser.dropMacroMeta(module)
    val doc      = DocParserRunner.createDocs(dropMeta)
    val code     = DocParserHTMLGenerator.generateHTMLForEveryDocumented(doc)
    code
  }

  /** Generates HTML from Documentation string.
    */
  def runOnPureDoc(comment: String): String = {
    val doc  = DocParser.runMatched(comment)
    val html = DocParserHTMLGenerator.generateHTMLPureDoc(doc)
    html
  }

  // TODO: Move'em to separate module.

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
        HTML.div(HTML.`class` := "main ml-20")(astName, html).render
      case None => HTML.div(astName, html).render
    }
  }

  /** Glues together many docs with AST elements in proper order to create one
    * documentation page.
    */
  def glueGeneratedContent(docs: List[String], astList: List[AST]): String = {
    docs.zip(astList).map(e => connectHtmlToAst(e._1, e._2)).mkString
  }

  /** Called if file doesn't contain docstrings, to let user know that they
    * won't find anything at this page, and that it is not a bug.
    */
  def mapIfEmpty(doc: String): String = {
    var tmp = doc
    if (doc.replace("<div>", "").replace("</div>", "").length == 0) {
      tmp =
        "\n\n*Enso Reference Viewer.*\n\nNo documentation available for chosen source file."
      tmp = runOnPureDoc(tmp).replace("style=\"font-size: 13px;\"", "")
    }
    tmp
  }

  /** Doc Parser may output file with many nested empty divs.
    * This simple function will remove all unnecessary HTML tags.
    */
  def removeUnnecessaryDivs(doc: String): String = {
    var tmp = doc
    while (tmp.contains("<div></div>"))
      tmp = tmp.replace("<div></div>", "")
    tmp
  }

  /** Traverses through root directory, outputs list of all accessible files.
    */
  def traverse(root: File): LazyList[File] =
    if (!root.exists) { LazyList.empty }
    else {
      LazyList.apply(root) ++ (root.listFiles match {
        case null  => LazyList.empty
        case files => files.view.flatMap(traverse)
      })
    }
}
