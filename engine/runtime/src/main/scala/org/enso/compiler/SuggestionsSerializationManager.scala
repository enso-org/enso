package org.enso.compiler

import com.oracle.truffle.api.TruffleLogger
import com.oracle.truffle.api.TruffleFile
import org.enso.pkg.Package
import org.enso.polyglot.Suggestion

import java.util.logging.Level
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.ClassTagExtensions
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import java.nio.charset.StandardCharsets

final class SuggestionsSerializationManager(logger: TruffleLogger) {

  /** The debug logging level. */
  private val debugLogLevel = Level.FINE

  private lazy val mapper = {
    val mapper  = new ObjectMapper() with ClassTagExtensions
    mapper.registerModule(DefaultScalaModule)
  }

  case class SuggetionWithVersion (version : Int, suggestions : Iterable[Suggestion])

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
      val w    = f.newBufferedWriter(StandardCharsets.UTF_8)
      w.write(toJson(suggestions))
      w.close()
    } finally {
      System.err.println("Out in " + f)
    }
  }

  def toJson(suggestions: Iterable[Suggestion]): String = new String(mapper
    .writerWithDefaultPrettyPrinter()
    .writeValueAsBytes(new SuggetionWithVersion(5068, suggestions)))

  def deserialize(pkg: Package[TruffleFile]): Unit = {
    logger.log(debugLogLevel, "deserialize [{}]", pkg.config.name)
    var f: TruffleFile = null
    try {
      val r: TruffleFile = pkg.root.resolve(pkg.config.name)
      r.createDirectories()
      f = r.resolve("suggestion.json")
      new String(f.readAllBytes, StandardCharsets.UTF_8)
    } finally {
      System.err.println("Loaded from " + f)
    }
  }

}
