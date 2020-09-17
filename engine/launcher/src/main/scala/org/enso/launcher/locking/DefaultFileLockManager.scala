package org.enso.launcher.locking
import java.nio.file.Path

import org.enso.launcher.installation.DistributionManager

/**
  * Default [[FileLockManager]] storing lock files in a directory defined by the
  * [[DistributionManager]].
  */
object DefaultFileLockManager extends FileLockManager {

  /**
    * @inheritdoc
    */
  override def locksRoot: Path = DistributionManager.paths.locks
}
