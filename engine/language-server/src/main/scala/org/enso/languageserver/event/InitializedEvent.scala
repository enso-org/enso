package org.enso.languageserver.event

/** Event about the initialization of the language server component. */
sealed trait InitializedEvent extends Event

object InitializedEvent {

  case object SuggestionsRepoInitialized  extends InitializedEvent
  case object FileVersionsRepoInitialized extends InitializedEvent
  case object InitializationFinished      extends InitializedEvent
  case object InitializationFailed        extends InitializedEvent
}
