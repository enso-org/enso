package org.enso.projectmanager.protocol

import org.enso.logger.ReportLogsOnFailure
import org.enso.projectmanager.TestDistributionConfiguration
import org.enso.runtimeversionmanager.runner.JVMSettings
import org.enso.runtimeversionmanager.test.FakeReleases

class ProjectCreateHandleMissingRuntimeSpec
    extends ProjectCreateSpecBase
    with ReportLogsOnFailure {
  override val distributionConfiguration =
    new TestDistributionConfiguration(
      distributionRoot       = testDistributionRoot.toPath,
      engineReleaseProvider  = FakeReleases.engineReleaseProvider,
      runtimeReleaseProvider = FakeReleases.runtimeReleaseProvider,
      discardChildOutput     = !debugChildLogs
    ) {
      override def defaultJVMSettings: JVMSettings = JVMSettings(
        javaCommandOverride = None,
        jvmOptions          = Seq()
      )
    }

  override val engineToInstall = Some(defaultVersion)

  "project/create" should {
    behave like correctlyHandleMissingRuntimeInPresenceOfEngine()
  }
}
