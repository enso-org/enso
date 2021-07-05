package org.enso.docs.generator

import java.io.File
import org.enso.syntax.text.{DocParser, Parser}
import org.enso.syntax.text.docparser._

/** Defines useful wrappers for Doc Parser.
  */
object DocParserWrapper {

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

  /** Called if file doesn't contain docstrings, to let user know that they
    * won't find anything at this page, and that it is not a bug.
    */
  def mapIfEmpty(doc: String): String = {
    if (doc.replace("<div>", "").replace("</div>", "").length == 0) {
      val placeholder =
        "\n\n*Enso Reference Viewer.*\n\nNo documentation available for chosen source file."
      val generatedPlaceholderCode =
        runOnPureDoc(placeholder).replace("style=\"font-size: 13px;\"", "")
      return generatedPlaceholderCode
    }
    doc
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
