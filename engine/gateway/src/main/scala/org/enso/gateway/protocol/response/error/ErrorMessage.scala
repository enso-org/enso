package org.enso.gateway.protocol.response.error

/** Messages of [[org.enso.gateway.protocol.response.ResponseError]]. */
object ErrorMessage {
  val invalidJson         = "Invalid JSON"
  val wrongJsonRpcVersion = "Wrong JSON-RPC Version"
  val methodNotFound      = "Method not found"
  val unexpectedError     = "Unexpected error"
}
