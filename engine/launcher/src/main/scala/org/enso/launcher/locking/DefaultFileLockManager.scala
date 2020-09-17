package org.enso.launcher.locking
import java.nio.file.Path

import org.enso.launcher.installation.DistributionManager

object DefaultFileLockManager extends FileLockManager {
  override def locksRoot: Path = DistributionManager.paths.locks
}
