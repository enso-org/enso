package org.enso.compiler

import com.oracle.truffle.api.TruffleLogger
import org.enso.pkg.Package
import org.enso.polyglot.Suggestion

import java.util.logging.Level

final class SuggestionsSerializationManager(logger: TruffleLogger) {

  /** The debug logging level. */
  private val debugLogLevel = Level.FINE

  def serialize[F](
    suggestions: Iterable[Suggestion],
    pkg: Package[F]
  ): Unit = {
    logger.log(
      debugLogLevel,
      "Serialize [{}, {}]",
      Array(pkg.config.name, suggestions.size)
    )
  }

  def deserialize[F](pkg: Package[F]): Unit = {
    logger.log(debugLogLevel, "deserialize [{}]", pkg.config.name)
  }

}
