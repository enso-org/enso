package org.enso.syntax.text

import cats.implicits._
import org.enso.syntax.text.AST.App

/** This preprocessor step is responsible for hoisting occurrences of `in` as a
  * variable identifier to occurrences of an _operator_ identifier, letting it
  * behave properly in the syntax.
  */
case object InHoisting {
  private val inName: String = "in"

  /** Executes the hoisting procedure on the provided token stream.
    *
    * @param tokenStream the input token stream
    * @return `tokenStream` with all occurrences of `Var("in")` replaced with
    *         `Opr("in")`
    */
  def run(tokenStream: AST.Module): AST.Module = {
    tokenStream.setLines(
      tokenStream.lines.map(l => l.copy(elem = l.elem.map(process)))
    )
  }

  /** Maps over the token stream, replacing occurrences of `Var("in")` with
    * `Opr("in")` at the same location.
    *
    * @param ast the ast element to process
    * @return `ast`, with occurrences of "in" replaced
    */
  def process(ast: AST): AST = ast match {
    case App.Prefix.any(app) =>
      App.Prefix(process(app.func), app.off, process(app.arg))
    case AST.Ident.Var.any(v) =>
      if (v.name == inName) {
        v.copy(
          shape = Shape.Opr("in")
        )
      } else {
        v
      }
    case AST.Block.any(block) =>
      val newFirstLine = block.firstLine.map(process)
      val newLines     = block.lines.map(_.map(_.map(process)))

      block.replaceFirstLine(newFirstLine).replaceLines(newLines)
    case n => n.map(process)
  }
}
