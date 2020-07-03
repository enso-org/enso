package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import io.circe.parser._

import scala.annotation.nowarn

object PingMatcher {

  @nowarn("cat=unused-params&msg=method")
  def unapply(arg: String): Option[UUID] = {
    val maybeJson = parse(arg).toOption

    for {
      json   <- maybeJson
      method <- json.hcursor.downField("method").as[String].toOption
      if method == "heartbeat/ping"
      id <- json.hcursor.downField("id").as[String].toOption
    } yield UUID.fromString(id)
  }

}
