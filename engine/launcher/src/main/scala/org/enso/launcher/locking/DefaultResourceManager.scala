package org.enso.launcher.locking

import org.enso.launcher.installation.DistributionManager

object DefaultResourceManager
    extends ResourceManager(new FileLockManager(DistributionManager))
