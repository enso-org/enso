package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import io.circe.parser._

object PingMatcher {

  def unapply(arg: String): Option[UUID] = {
    val maybeJson = parse(arg).toOption

    for {
      json <- maybeJson
      method <- json.hcursor
        .downField("method")
        .as[String]
        .toOption if method == "heartbeat/ping"
      id <- json.hcursor.downField("id").as[String].toOption
    } yield UUID.fromString(id)
  }

}
