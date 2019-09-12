package org.enso.syntax.text

import java.io.File
import java.io.PrintWriter

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.spec.DocParserDef
import scalatags.Text.TypedTag

import scala.util.Random

////////////////////////////////////////////////////////////////////////////////
//// DocParser /////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class DocParser {
  import DocParser._
  private val engine = newEngine()

  def parserRun(input: String) = run(input) match {
    case flexer.Parser.Result(_, flexer.Parser.Result.Success(v)) =>
      println(v.renderHTML("style.css"))
      val path =
        "syntax/specialization/src/main/scala/org/enso/syntax/text/DocParserHTMLOut/"
      saveHTMLCodeToLocalFile(path, v.renderHTML("style.css"))
      v
    case _ => Doc()
  }
  def run(input: String): Result[Doc] = engine.run(new Reader(input))

}

object DocParser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(DocParserDef())

  def parserRun(input: String) = new DocParser().parserRun(input)
  def run(input: String): Result[Doc] = new DocParser().run(input)

  def saveHTMLCodeToLocalFile(path: String, code: TypedTag[String]): Unit = {
    val writer = new PrintWriter(
      new File(
        path + Random.alphanumeric.take(8).mkString("") + ".html"
      )
    )
    writer.write(code.toString)
    writer.close()
  }
}
