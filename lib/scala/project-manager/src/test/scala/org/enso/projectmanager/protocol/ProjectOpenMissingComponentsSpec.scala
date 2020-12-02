package org.enso.projectmanager.protocol

import java.io.File
import java.util.UUID

import akka.testkit.TestActors.blackholeProps
import io.circe.Json
import io.circe.literal.JsonStringContext
import nl.gn0s1s.bump.SemVer
import org.enso.pkg.SemVerEnsoVersion
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.testkit.RetrySpec
import zio.Runtime

class ProjectOpenMissingComponentsSpec
    extends BaseServerSpec
    with RetrySpec
    with ProjectManagementOps
    with MissingComponentBehavior {
  val ordinaryVersion                          = SemVer(0, 0, 1)
  override val engineToInstall: Option[SemVer] = Some(ordinaryVersion)
  var ordinaryProject: UUID                    = _
  var brokenProject: UUID                      = _

  override val deleteProjectsRootAfterEachTest = false
  override def beforeAll(): Unit = {
    super.beforeAll()

    val blackhole = system.actorOf(blackholeProps)
    val ordinaryAction = projectService.createUserProject(
      blackhole,
      "proj1",
      ordinaryVersion,
      MissingComponentAction.Fail
    )
    ordinaryProject = Runtime.default.unsafeRun(ordinaryAction)
    val brokenName = "projbroken"
    val brokenAction = projectService.createUserProject(
      blackhole,
      brokenName,
      ordinaryVersion,
      MissingComponentAction.Fail
    )
    brokenProject = Runtime.default.unsafeRun(brokenAction)

    // TODO [RW] this hack should not be necessary with #1273
    val projectDir = new File(userProjectDir, brokenName)
    val pkgManager = org.enso.pkg.PackageManager.Default
    val pkg        = pkgManager.loadPackage(projectDir).get
    pkg.updateConfig(config =>
      config.copy(ensoVersion =
        SemVerEnsoVersion(SemVer(0, 999, 0, Some("broken")))
      )
    )

    uninstallEngine(ordinaryVersion)
  }

  override def buildRequest(
    version: SemVer,
    missingComponentAction: MissingComponentAction
  ): Json = {
    val projectId =
      if (version.preRelease.contains("broken")) brokenProject
      else ordinaryProject

    json"""
        { "jsonrpc": "2.0",
          "method": "project/open",
          "id": 1,
          "params": {
            "projectId": $projectId,
            "missingComponentAction": $missingComponentAction
          }
        }
        """
  }

  override def isSuccess(json: Json): Boolean = {
    val result = for {
      obj    <- json.asObject
      result <- obj("result").flatMap(_.asObject)
    } yield result
    result.isDefined
  }

  "project/open" should {
    behave like correctlyHandleMissingComponents()
  }
}
