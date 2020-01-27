package org.enso.languageserver

/** Akka messages sent by Gateway received LSP notifications. */
object Notifications {

  /** Akka message sent by Gateway received LSP notification `initialized`. */
  case object Initialized

  /** Akka message sent by Gateway received LSP notification `exit`. */
  case object Exit

}
