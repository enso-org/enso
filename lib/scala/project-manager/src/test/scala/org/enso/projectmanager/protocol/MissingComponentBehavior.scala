package org.enso.projectmanager.protocol

import io.circe.Json
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.BaseServerSpec
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.runtimeversionmanager.components.GraalVMVersion
import org.enso.testkit.RetrySpec
import org.scalatest.wordspec.AnyWordSpecLike

trait MissingComponentBehavior {
  this: BaseServerSpec with AnyWordSpecLike with RetrySpec =>
  def buildRequest(
    version: SemVer,
    missingComponentAction: MissingComponentAction
  ): Json

  def isSuccess(json: Json): Boolean

  val defaultVersion = SemVer(0, 0, 1)
  val brokenVersion  = SemVer(0, 9999, 0, Some("broken"))

  def correctlyHandleMissingComponents(): Unit = {
    "fail if a missing version is requested with Fail" in {
      val client = new WsTestClient(address)
      client.send(buildRequest(defaultVersion, MissingComponentAction.Fail))
      client.expectError(4020)
    }

    "install the missing version and succeed with Install" taggedAs Retry in {
      val client = new WsTestClient(address)
      client.send(
        buildRequest(defaultVersion, MissingComponentAction.Install)
      )

      /** We do not check for success here as we are concerned onyl that the
        * installation is attempted. Installation and creating/opening projects
        * are tested elsewhere.
        */
      client.expectTaskStarted()
    }

    "fail if the requested missing version is marked as broken with " +
    "Install" in {
      val client = new WsTestClient(address)
      client.send(buildRequest(brokenVersion, MissingComponentAction.Install))
      client.expectError(4021)
    }

    "succeed even if the requested missing version is marked as broken " +
    "with ForceInstallBroken" taggedAs Retry in {
      val client = new WsTestClient(address)
      client.send(
        buildRequest(brokenVersion, MissingComponentAction.ForceInstallBroken)
      )
      client.expectTaskStarted()
    }
  }

  /** This behaviour should be tested in a separate test suite, as it affects
    *  the test environment and if run together with other tests it could affect
    *  their results.
    */
  def correctlyHandleMissingRuntimeInPresenceOfEngine(): Unit = {
    "make sure to check if the runtime is installed even if the engine was " +
    "already installed" in {
      uninstallRuntime(GraalVMVersion("2.0.0", "11"))

      val client = new WsTestClient(address)
      client.send(
        buildRequest(defaultVersion, MissingComponentAction.Install)
      )

      /** We do not check for success here as we are concerned onyl that the
        * installation is attempted. Installation and creating/opening projects
        * are tested elsewhere.
        */
      client.expectTaskStarted()
    }
  }
}
