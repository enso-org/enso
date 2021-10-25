package org.enso.launcher.upgrade

import java.nio.file.Path

object FakeLauncherReleases {

  /** Location of fake launcher releases used for testing upgrades. */
  def path: Path = Path.of(
    getClass
      .getResource("/org/enso/launcher/components/fake-releases/launcher")
      .toURI
  )
}
