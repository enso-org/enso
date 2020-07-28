package org.enso.launcher.installation

import org.enso.launcher.Environment
import org.enso.launcher.internal.installation.DistributionManager

/**
  * A default DistributionManager using the default environment.
  */
object DistributionManager extends DistributionManager(Environment)
