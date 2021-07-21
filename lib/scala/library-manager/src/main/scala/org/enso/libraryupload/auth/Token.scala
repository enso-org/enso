package org.enso.libraryupload.auth

trait Token {
  // TODO add to request
}

case class SimpleHeaderToken(headerName: String, value: String) extends Token
