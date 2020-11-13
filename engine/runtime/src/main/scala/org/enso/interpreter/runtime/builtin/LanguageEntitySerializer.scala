package org.enso.interpreter.runtime.builtin

import io.circe.Json
import org.enso.interpreter.runtime.callable.atom.{Atom, AtomConstructor}

/**
  * Helper for JSON-serializing runtime entities of the language.
  */
object LanguageEntitySerializer {

  /**
    * Serializes a language entity into a JSON string. Returns null JSON for
    * unexpected entities.
    *
    * @param obj any object representing an Enso language entity.
    * @return the JSON string representing `obj` or `"null"` if the object
    *         is not a serializable language entity.
    */
  final def serialize(obj: Object): String = toJson(obj).noSpaces

  private def toJson(obj: Any): Json = obj match {
    case l: Long   => Json.fromLong(l)
    case s: String => Json.fromString(s)
    case cons: AtomConstructor =>
      Json.obj("type" -> Json.fromString(cons.getName), "fields" -> Json.arr())
    case atom: Atom =>
      Json.obj(
        "type"   -> Json.fromString(atom.getConstructor.getName),
        "fields" -> Json.arr(atom.getFields.map(toJson).toIndexedSeq: _*)
      )
    case _ => Json.Null
  }
}
