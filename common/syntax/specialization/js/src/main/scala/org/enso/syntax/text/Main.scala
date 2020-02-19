package org.enso.syntax.text

import org.enso.flexer.Reader
import org.enso.syntax.text

import io.circe.parser._
import scala.scalajs.js.annotation._
import scala.scalajs.js

object Parse {
  @JSExportTopLevel("parse")
  def parse(program: String, idsJson: String): String = {
    try {
      val ids = decode[Seq[((Int, Int), AST.ID)]](idsJson).getOrElse {
        throw new Exception("Could not decode IDMap from json.")
      }
      val ast = new text.Parser().run(new Reader(program), text.Parser.idMap(ids))
      ast.toJson().noSpacesSortKeys
    } catch {
      // FIXME We wrap the error message in JavaScriptException, so that javascript
      //  can display it. This is no longer needed in scalajs 1.0
      case e: Throwable => throw js.JavaScriptException(e.getMessage)
    }
  }
}
