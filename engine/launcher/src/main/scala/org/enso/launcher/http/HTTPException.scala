package org.enso.launcher.http

/** Indicates an error when processing a HTTP request.
  */
case class HTTPException(message: String) extends RuntimeException(message)
