package org.enso.syntax.text

import io.circe.syntax._
import org.enso.flexer.Reader
import org.enso.syntax.text.Parser.ParserError

import scala.scalajs.js.annotation._
import scala.scalajs.js

object Parse {
  @JSExportTopLevel("parse")
  def parse(program: String, idsJson: String): String = {
    try {
      val ids = Parser.idMapFromJson(idsJson).left.map { error =>
          throw new ParserError("Could not deserialize idmap.", error)
        }.merge
      new Parser().run(new Reader(program), ids).toJson().noSpacesSortKeys
    } catch {
      // FIXME We wrap the error message in JavaScriptException, so that javascript
      //  can display it. This is no longer needed in scalajs 1.0
      case e: Throwable => throw js.JavaScriptException(e.getMessage)
    }
  }
  @JSExportTopLevel("parse_with_metadata")
  def parse_with_metadata(program: String): String = {
    try {
      new Parser().runWithMetadata(program).asJson.noSpacesSortKeys
    } catch {
      // FIXME We wrap the error message in JavaScriptException, so that javascript
      //  can display it. This is no longer needed in scalajs 1.0
      case e: Throwable => throw js.JavaScriptException(e.getMessage)
    }
  }
}
