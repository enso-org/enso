package org.enso.docsgenerator

import java.io.File
import org.enso.syntax.text.{DocParser, DocParserMain, Parser}

object DocsGenerator {
  def generate(program: String): String = {
    val parser   = new Parser()
    val module   = parser.run(program)
    val dropMeta = parser.dropMacroMeta(module)
    val doc      = DocParser.DocParserRunner.createDocs(dropMeta)
    val code =
      DocParser.DocParserHTMLGenerator.generateHTMLForEveryDocumented(doc)
    code
  }

  def generatePure(comment: String): String = {
    val doc  = DocParserMain.runMatched(comment)
    val html = DocParser.DocParserHTMLGenerator.generateHTMLPureDoc(doc)
    html
  }

  def mapIfEmpty(doc: String): String = {
    var tmp = doc
    if (doc.replace("<div>", "").replace("</div>", "").length == 0) {
      tmp =
        "\n\n*Enso Reference Viewer.*\n\nNo documentation available for chosen source file."
      tmp = generatePure(tmp).replace("style=\"font-size: 13px;\"", "")
    }
    tmp
  }

  def removeUnnecessaryDivs(doc: String): String = {
    var tmp = doc
    while (tmp.contains("<div></div>"))
      tmp = tmp.replace("<div></div>", "")
    tmp
  }

  def traverse(root: File): LazyList[File] =
    if (!root.exists) LazyList.empty
    else
      LazyList.apply(root) ++ (root.listFiles match {
        case null  => LazyList.empty
        case files => files.view.flatMap(traverse)
      })
}
