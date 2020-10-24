package org.enso.launcher

import org.enso.componentmanager.{DistributionManager, GlobalCLIOptions}
import org.enso.componentmanager.components.ComponentManager
import org.enso.componentmanager.locking.DefaultResourceManager
import org.enso.componentmanager.releases.runtime.GraalCEReleaseProvider
import org.enso.launcher.cli.CLIComponentManagementUserInterface
import org.enso.launcher.releases.EnsoRepository

object DefaultComponentManager {
  def make(globalCLIOptions: GlobalCLIOptions): ComponentManager =
    new ComponentManager(
      new CLIComponentManagementUserInterface(globalCLIOptions),
      DistributionManager,
      DefaultResourceManager,
      EnsoRepository.defaultEngineReleaseProvider,
      GraalCEReleaseProvider
    )
}
