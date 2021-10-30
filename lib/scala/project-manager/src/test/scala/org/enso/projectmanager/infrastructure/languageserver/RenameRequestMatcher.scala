package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import io.circe.Json
import io.circe.parser.parse

import scala.annotation.nowarn

object RenameRequestMatcher {

  @nowarn("cat=unused-params&msg=method")
  def unapply(arg: String): Option[(UUID, String, String, String)] = {
    val maybeJson = parse(arg).toOption

    for {
      json   <- maybeJson
      method <- json.hcursor.downField("method").as[String].toOption
      if method == "refactoring/renameProject"
      id        <- json.hcursor.downField("id").as[String].toOption
      params    <- json.hcursor.downField("params").as[Json].toOption
      namespace <- params.hcursor.downField("namespace").as[String].toOption
      oldName   <- params.hcursor.downField("oldName").as[String].toOption
      newName   <- params.hcursor.downField("newName").as[String].toOption
    } yield (UUID.fromString(id), namespace, oldName, newName)
  }

}
