package org.enso.languageserver.event

/** Event about the initialization of the language server component. */
sealed trait InitializedEvent extends Event

object InitializedEvent {

  case object SuggestionsRepo  extends InitializedEvent
  case object FileVersionsRepo extends InitializedEvent
}
