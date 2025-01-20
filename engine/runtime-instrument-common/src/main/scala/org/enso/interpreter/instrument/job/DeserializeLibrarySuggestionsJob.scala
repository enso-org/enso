package org.enso.interpreter.instrument.job

import org.enso.editions.LibraryName
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level

import scala.jdk.CollectionConverters._
import org.enso.polyglot.Suggestion

/** A job responsible for deserializing suggestions of loaded library.
  *
  * @param libraryName the name of loaded library
  */
final class DeserializeLibrarySuggestionsJob(
  val libraryName: LibraryName
) extends BackgroundJob[Unit](DeserializeLibrarySuggestionsJob.Priority, true)
    with UniqueJob[Unit] {

  /** @inheritdoc */
  override def equalsTo(that: UniqueJob[_]): Boolean =
    that match {
      case that: DeserializeLibrarySuggestionsJob =>
        this.libraryName == that.libraryName
      case _ => false
    }

  /** @inheritdoc */
  override def runImpl(implicit ctx: RuntimeContext): Unit = {
    ctx.executionService.getLogger.log(
      Level.FINE,
      "Deserializing suggestions for library [{}].",
      libraryName
    )
    val cc = ctx.executionService.getContext.getCompiler.context
    cc
      .deserializeSuggestions(libraryName)
      .foreach { cachedSuggestions =>
        val suggestions =
          cachedSuggestions.asInstanceOf[java.util.List[Suggestion]]
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.SuggestionsDatabaseSuggestionsLoadedNotification(
              libraryName,
              suggestions.asScala.toVector
            )
          )
        )
      }
  }

  override def toString: String =
    s"DeserializeLibrarySuggestionsJob($libraryName)"
}

object DeserializeLibrarySuggestionsJob {

  private val Priority = 100
}
