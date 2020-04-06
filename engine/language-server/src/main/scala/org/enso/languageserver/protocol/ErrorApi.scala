package org.enso.languageserver.protocol

import org.enso.jsonrpc.Error

/**
  * Generic errors provided by the language server.
  *
  * @see [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md#errors---language-server]]
  */
object ErrorApi {

  case object AccessDeniedError extends Error(100, "Access denied")

}
