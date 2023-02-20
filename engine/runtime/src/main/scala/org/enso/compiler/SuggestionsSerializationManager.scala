package org.enso.compiler

import com.oracle.truffle.api.TruffleLogger
import com.oracle.truffle.api.TruffleFile
import org.enso.pkg.Package
import org.enso.polyglot.Suggestion

import java.util.logging.Level
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.ClassTagExtensions
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.JsonFactory

final class SuggestionsSerializationManager(logger: TruffleLogger) {

  /** The debug logging level. */
  private val debugLogLevel = Level.FINE

  private lazy val mapper = {
    val factory = new JsonFactory()
    val mapper  = new ObjectMapper(factory) with ClassTagExtensions
    mapper.registerModule(DefaultScalaModule)
  }

  def serialize(
    suggestions: Iterable[Suggestion],
    pkg: Package[TruffleFile]
  ): Unit = {
    logger.log(
      debugLogLevel,
      "Serialize [{}, {}]",
      Array(pkg.config.name, suggestions.size)
    )
    var f: TruffleFile = null
    try {
      val r: TruffleFile = pkg.root.resolve(pkg.config.name)

      r.createDirectories()
      f = r.resolve("suggestion.json")
      val json = mapper.writeValueAsString(suggestions)
      val w    = f.newBufferedWriter()
      w.write(json)
      w.close()
    } finally {
      System.err.println("Out in " + f)
    }

  }

  def deserialize(pkg: Package[TruffleFile]): Unit = {
    logger.log(debugLogLevel, "deserialize [{}]", pkg.config.name)
    throw new UnsupportedOperationException("deserialize: " + pkg.root)
  }

}
