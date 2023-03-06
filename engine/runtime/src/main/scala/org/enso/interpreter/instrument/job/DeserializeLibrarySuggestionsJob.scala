package org.enso.interpreter.instrument.job

import org.enso.editions.LibraryName
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level

import scala.jdk.CollectionConverters._

/** A job responsible for deserializing suggestions of loaded library.
  *
  * @param libraryName the name of loaded library
  */
final class DeserializeLibrarySuggestionsJob(
  libraryName: LibraryName
) extends Job[Unit](
      List(),
      isCancellable         = false,
      mayInterruptIfRunning = false
    ) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.executionService.getLogger.log(
      Level.FINE,
      s"Deserializing suggestions for library [$libraryName]."
    )
    val serializationManager =
      ctx.executionService.getContext.getCompiler.getSerializationManager
    serializationManager
      .deserializeSuggestions(libraryName)
      .foreach { cachedSuggestions =>
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.SuggestionsDatabaseSuggestionsLoadedNotification(
              libraryName,
              cachedSuggestions.getSuggestions.asScala.toVector
            )
          )
        )
      }
  }
}
