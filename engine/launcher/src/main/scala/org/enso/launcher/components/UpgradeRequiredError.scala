package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer
import org.enso.componentmanager.CurrentVersion
import org.enso.componentmanager.components.ComponentsException
import org.enso.launcher.cli.GlobalCLIOptions

// TODO [RW] rename this to not have launcher
// TODO [RW] and probably move to CM but need to get rid of CLI options first somehow
/** Indicates that a requested engine version requires a newer launcher version.
  *
  * This error can be recovered by
  * [[org.enso.launcher.upgrade.LauncherUpgrader.recoverUpgradeRequiredErrors]]
  * which can perform the upgrade and re-run the requested command with the
  * newer version.
  *
  * @param expectedLauncherVersion the minimum launcher version that is required
  * @param globalCLIOptions the CLI options that should be passed to an upgrader
  *                         if an upgrade is requested
  */
case class LauncherUpgradeRequiredError(
  expectedLauncherVersion: SemVer,
  globalCLIOptions: GlobalCLIOptions
) extends ComponentsException(
      s"Minimum launcher version required to use this engine is " +
      s"$expectedLauncherVersion"
    ) {

  /** @inheritdoc
    */
  override def toString: String =
    s"This launcher version is ${CurrentVersion.version}, but " +
    s"$expectedLauncherVersion is required to run this engine. If you want " +
    s"to use it, upgrade the launcher with `enso upgrade`."

}
