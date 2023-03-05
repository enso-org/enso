package org.enso.interpreter.instrument.job

import org.enso.editions.LibraryName
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

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
    val serializationManager =
      ctx.executionService.getContext.getCompiler.getSerializationManager
    serializationManager
      .deserializeSuggestions(libraryName)
      .foreach { cachedSuggestions =>
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.SuggestionsDatabaseSuggestionsLoadedNotification(
              cachedSuggestions.getSuggestions.asScala.toVector
            )
          )
        )
      }
  }
}
