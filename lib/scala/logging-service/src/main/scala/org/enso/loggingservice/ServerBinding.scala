package org.enso.loggingservice

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.{Authority, Host, Path}

case class ServerBinding(port: Int) {
  def toUri(host: String = "localhost"): Uri =
    Uri(
      scheme    = "ws",
      authority = Authority(host = Host(host), port = port),
      path      = Path./
    )
}
