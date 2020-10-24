package org.enso.componentmanager.components

import nl.gn0s1s.bump.SemVer
import org.enso.componentmanager.CurrentVersion

// TODO [RW] and probably move to CM but need to get rid of CLI options first somehow
/** Indicates that a requested engine version requires a newer launcher version.
  *
  * @param expectedLauncherVersion the minimum launcher version that is required
  */
case class UpgradeRequiredError(
  expectedLauncherVersion: SemVer
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
