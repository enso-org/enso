package org.enso.projectmanager.protocol

import io.circe.Json
import io.circe.literal.JsonStringContext
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.testkit.RetrySpec
import org.enso.editions.SemVerJson._

abstract class ProjectCreateSpecBase
    extends BaseServerSpec
    with RetrySpec
    with ProjectManagementOps
    with MissingComponentBehavior {
  override def buildRequest(
    version: SemVer,
    missingComponentAction: MissingComponentAction
  ): Json =
    json"""
        { "jsonrpc": "2.0",
          "method": "project/create",
          "id": 1,
          "params": {
            "name": "Testproj",
            "missingComponentAction": $missingComponentAction,
            "version": $version
          }
        }
        """

  override def isSuccess(json: Json): Boolean = {
    val projectId = for {
      obj    <- json.asObject
      result <- obj("result").flatMap(_.asObject)
      id     <- result("projectId")
    } yield id
    projectId.isDefined
  }
}
