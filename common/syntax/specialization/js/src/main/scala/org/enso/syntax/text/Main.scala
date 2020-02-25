package org.enso.syntax.text

import org.enso.flexer.Reader

import scala.scalajs.js.annotation._
import scala.scalajs.js

object Parse {
  @JSExportTopLevel("parse")
  def parse(program: String, idsJson: String): String = {
    try {
      val ids = Parser.idMapFromJson(idsJson).getOrElse {
        throw new Exception("Could not decode IDMap from json.")
      }
      new Parser().run(new Reader(program), ids).toJson().noSpacesSortKeys
    } catch {
      // FIXME We wrap the error message in JavaScriptException, so that javascript
      //  can display it. This is no longer needed in scalajs 1.0
      case e: Throwable => throw js.JavaScriptException(e.getMessage)
    }
  }
}
