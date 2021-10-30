package org.enso.projectmanager.requesthandler

import org.enso.jsonrpc.Error

/** Defines a mapping from an internal failure type to protocol error messages.
  */
trait FailureMapper[FailureType] {

  /** Converts a [[FailureType]] into the protocol [[Error]] message. */
  def mapFailure(failure: FailureType): Error
}
