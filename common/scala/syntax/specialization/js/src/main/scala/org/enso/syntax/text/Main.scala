package org.enso.syntax.text

import org.enso.flexer.Reader

import scala.scalajs.js.annotation._
import scala.scalajs.js

object Parse {
    @JSExportTopLevel("parse")
    def parse(input: String): String = {
      try {
        new Parser().run(new Reader(input)).toJson().noSpacesSortKeys
      } catch {
        // FIXME We wrap the error message in JavaScriptException, so that javascript
        //  can display it. This is no longer needed in scalajs 1.0
        case e : Throwable => throw js.JavaScriptException(e.getMessage)
      }
    }

    @JSExportTopLevel("parseWithIDs")
    def parse(input: String, idsJSON: String): String = {
      try {
        val ids: Parser.IDMap = ??? // TODO [Josef] decode from JSON
        new Parser().run(new Reader(input), ids).toJson().noSpacesSortKeys
      } catch {
        // FIXME We wrap the error message in JavaScriptException, so that javascript
        //  can display it. This is no longer needed in scalajs 1.0
        case e : Throwable => throw js.JavaScriptException(e.getMessage)
      }
    }
}
