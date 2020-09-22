package org.enso.launcher.http

case class HTTPException(message: String) extends RuntimeException(message)
