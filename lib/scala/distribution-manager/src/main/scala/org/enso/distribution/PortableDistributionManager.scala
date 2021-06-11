package org.enso.distribution

import com.typesafe.scalalogging.Logger
import org.enso.distribution.FileSystem.PathSyntax

import java.nio.file.{Files, Path}

/** A specialized variant of [[DistributionManager]] that is able to detect if
  * the currently running distribution is running in portable or locally
  * installed mode.
  */
class PortableDistributionManager(env: Environment)
    extends DistributionManager(env) {
  private val logger = Logger[PortableDistributionManager]

  /** Name of the file that should be placed in the distribution root to mark it
    * as running in portable mode.
    */
  private val PORTABLE_MARK_FILENAME = ".enso.portable"

  /** Describes the path of a possible distribution root.
    *
    * This directory is checked for [[PORTABLE_MARK_FILENAME]]. If the mark file
    * is present, portable mode is selected.
    */
  def possiblePortableRoot: Path =
    env.getPathToRunningExecutable.getParent.getParent

  /** Specifies whether the program has been run as a portable distribution or
    * it is a locally installed distribution.
    */
  lazy val isRunningPortable: Boolean = {
    val portable = detectPortable()
    logger.debug("Launcher portable [mode={}].", portable)
    if (portable && LocallyInstalledDirectories.installedDistributionExists) {
      val installedRoot   = LocallyInstalledDirectories.dataDirectory
      val installedBinary = LocallyInstalledDirectories.binaryExecutable

      logger.debug(
        "The launcher is run in portable mode, but an installed distribution" +
        " is available at [{}].",
        installedRoot
      )

      if (Files.exists(installedBinary)) {
        if (installedBinary == env.getPathToRunningExecutable) {
          logger.debug(
            "That distribution seems to be corresponding to this launcher " +
            "executable, that is running in portable mode."
          )
        } else {
          logger.debug(
            "However, that installed distribution most likely uses another " +
            "launcher executable, located at [{}].",
            installedBinary
          )
        }
      }
    }
    portable
  }

  /** Detects paths for the portable distribution.
    *
    * A portable distribution does not include bundle paths, because if
    * anything was bundled with it, it is already part of its primary installation.
    */
  override protected def detectPaths(): DistributionPaths =
    if (isRunningPortable) {
      val root = env.getPathToRunningExecutable.getParent.getParent
      val home = detectEnsoHome()
      import DistributionManager._
      DistributionPaths(
        dataRoot                  = root,
        runtimes                  = root / RUNTIMES_DIRECTORY,
        engines                   = root / ENGINES_DIRECTORY,
        bundle                    = None,
        config                    = root / CONFIG_DIRECTORY,
        locks                     = root / LOCK_DIRECTORY,
        logs                      = root / LOG_DIRECTORY,
        unsafeTemporaryDirectory  = root / TMP_DIRECTORY,
        customEditions            = detectCustomEditionPaths(home),
        localLibrariesSearchPaths = detectLocalLibraryPaths(home),
        ensoHome                  = home
      )
    } else super.detectPaths()

  private def detectPortable(): Boolean = Files.exists(portableMarkFilePath)

  private def portableMarkFilePath: Path =
    possiblePortableRoot / PORTABLE_MARK_FILENAME
}
