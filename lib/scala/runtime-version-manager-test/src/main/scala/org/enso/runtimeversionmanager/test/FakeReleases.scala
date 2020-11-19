package org.enso.runtimeversionmanager.test

import java.nio.file.Path

import org.enso.runtimeversionmanager.releases.engine.EngineReleaseProvider
import org.enso.runtimeversionmanager.releases.graalvm.GraalCEReleaseProvider
import org.enso.runtimeversionmanager.releases.testing.FakeReleaseProvider

object FakeReleases {

  /** Location of fake engine and runtime releases used for testing component
    * installation.
    */
  def releaseRoot: Path = Path.of(getClass.getResource("fake-releases").toURI)

  /** Provider of engine releases using the test fixtures. */
  lazy val engineReleaseProvider = new EngineReleaseProvider(
    FakeReleaseProvider(
      releaseRoot.resolve("enso"),
      copyIntoArchiveRoot = Seq("manifest.yaml")
    )
  )

  /** Provider of Graal runtime releases using the test fixtures. */
  lazy val runtimeReleaseProvider = new GraalCEReleaseProvider(
    FakeReleaseProvider(releaseRoot.resolve("graalvm"))
  )
}
