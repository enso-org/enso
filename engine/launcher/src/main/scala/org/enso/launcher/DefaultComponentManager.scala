package org.enso.launcher

import org.enso.launcher.cli.{
  CLIRuntimeVersionManagementUserInterface,
  GlobalCLIOptions
}
import org.enso.launcher.distribution.DefaultManagers._
import org.enso.runtimeversionmanager.components.RuntimeVersionManager
import org.enso.runtimeversionmanager.releases.engine.EngineRepository
import org.enso.runtimeversionmanager.releases.graalvm.GraalCEReleaseProvider

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
      defaultResourceManager,
      EngineRepository.defaultEngineReleaseProvider,
      GraalCEReleaseProvider
    )
}
