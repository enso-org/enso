package org.enso.runtimeversionmanager.components

import java.nio.file.Files

import org.enso.runtimeversionmanager.FileSystem
import org.enso.runtimeversionmanager.FileSystem.PathSyntax
import org.enso.runtimeversionmanager.test.{
  FakeEnvironment,
  RuntimeVersionManagerTest,
  WithTemporaryDirectory
}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LocalReleaseProviderSpec extends RuntimeVersionManagerTest {

  private def prepareLocalRepository(
    engines: Seq[String],
    runtimes: Seq[String]
  ): Unit = {
    val localEngine = getTestDirectory / "offline-engine"
    val localGraal  = getTestDirectory / "offline-graal"
    Files.createDirectories(localEngine)
    Files.createDirectories(localGraal)
    for (engine <- engines) {}
  }

  "LocalReleaseProvider" should {
    "install a release from a local repository" in {
      val local = ???

    }

    "include releases from fallback" in {}
  }
}
