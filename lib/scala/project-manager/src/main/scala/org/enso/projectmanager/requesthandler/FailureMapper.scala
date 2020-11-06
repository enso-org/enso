package org.enso.projectmanager.requesthandler

import org.enso.jsonrpc.Error

trait FailureMapper[FailureType] {
  def mapFailure(failure: FailureType): Error
}
