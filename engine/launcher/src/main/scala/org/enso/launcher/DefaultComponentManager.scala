package org.enso.launcher

import org.enso.componentmanager.components.ComponentManager
import org.enso.componentmanager.releases.runtime.GraalCEReleaseProvider
import org.enso.launcher.cli.{
  CLIComponentManagementUserInterface,
  GlobalCLIOptions
}
import org.enso.launcher.distribution.DefaultManagers._
import org.enso.launcher.releases.EnsoRepository

object DefaultComponentManager {
  def make(
    globalCLIOptions: GlobalCLIOptions,
    alwaysInstallMissing: Boolean
  ): ComponentManager =
    new ComponentManager(
      new CLIComponentManagementUserInterface(
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
