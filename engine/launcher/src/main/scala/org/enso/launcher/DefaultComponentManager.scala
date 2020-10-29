package org.enso.launcher

import org.enso.runtimeversionmanager.components.RuntimeVersionManager
import org.enso.runtimeversionmanager.releases.graalvm.GraalCEReleaseProvider
import org.enso.launcher.cli.{
  CLIRuntimeVersionManagementUserInterface,
  GlobalCLIOptions
}
import org.enso.launcher.distribution.DefaultManagers._
import org.enso.launcher.releases.EnsoRepository

object DefaultComponentManager {
  def make(
    globalCLIOptions: GlobalCLIOptions,
    alwaysInstallMissing: Boolean
  ): RuntimeVersionManager =
    new RuntimeVersionManager(
      new CLIRuntimeVersionManagementUserInterface(
        globalCLIOptions,
        alwaysInstallMissing
      ),
      distributionManager,
      temporaryDirectoryManager,
      DefaultResourceManager,
      EnsoRepository.defaultEngineReleaseProvider,
      GraalCEReleaseProvider
    )
}
