package org.enso.syntax.text

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.spec.DocParserDef
import flexer.Parser.{Result => res}

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/** This is the class used to invoke Documentation Parser.
  *
  * It is used to create structured documentation from the blocks of commented
  * text created by the main Enso parser.
  *
  * It has been built on the same foundation as Parser, so in order not to
  * duplicate information, please refer to Parser documentation.
  */
class DocParser {
  import DocParser._
  private val engine = newEngine()
  private val errMsg = "Internal Documentation Parser Error"

  /** Used to match result of [[run]] function to possibly retrieve Doc
    *
    * @param input - input string to Doc Parser
    * @return - If it was able to retrieve Doc, then retrieved data, else
    *           exception with error message [[errMsg]]
    */
  def runMatched(input: String): Doc =
    run(input) match {
      case res(_, res.Success(v)) => v
      case _                      => throw new Exception(errMsg)
    }

  /** Used to initialize Doc Parser with input string to get parsed Doc
    *
    * @param input - input string to Doc Parser
    * @return - unmatched result possibly containing Doc
    */
  def run(input: String): Result[Doc] = engine.run(new Reader(input))
}

object DocParser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(DocParserDef())

  /** Doc Parser running methods, as described above, in class [[docparser]]
    */
  def runMatched(input: String): Doc = {
    val lines            = input.split("\n")
    var indentSecondLine = 0
    if (lines.tail.nonEmpty) {
      val s = lines.tail.head
      indentSecondLine = s.indexOf(s.trim())
    }
    val in = " " * indentSecondLine + lines.mkString("\n")
    new DocParser().runMatched(in)
  }
  def run(input: String): Result[Doc] = new DocParser().run(input)
}
