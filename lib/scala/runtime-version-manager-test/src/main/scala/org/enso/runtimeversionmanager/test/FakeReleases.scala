package org.enso.runtimeversionmanager.test

import java.nio.file.Path

object FakeReleases {

  /** Location of fake engine and runtime releases used for testing component
    * installation.
    */
  def path: Path = Path.of(getClass.getResource("fake-releases").toURI)
}
