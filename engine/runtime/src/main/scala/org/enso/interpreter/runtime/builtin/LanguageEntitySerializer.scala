package org.enso.interpreter.runtime.builtin

import com.oracle.truffle.api.interop.InteropLibrary
import io.circe.Json
import org.enso.interpreter.runtime.callable.atom.{Atom, AtomConstructor}

/**
  * Helper for JSON-serializing runtime entities of the language.
  */
object LanguageEntitySerializer {
  private val interopLibrary: InteropLibrary = InteropLibrary.getUncached()

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
    case cons: AtomConstructor =>
      Json.obj("type" -> Json.fromString(cons.getName), "fields" -> Json.arr())
    case atom: Atom =>
      Json.obj(
        "type"   -> Json.fromString(atom.getConstructor.getName),
        "fields" -> Json.arr(atom.getFields.map(toJson).toIndexedSeq: _*)
      )
    case _ =>
      if (interopLibrary.isString(obj)) {
        Json.fromString(interopLibrary.asString(obj))
      } else {
        Json.Null
      }
  }
}
