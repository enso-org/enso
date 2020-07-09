package org.enso.launcher

import java.nio.file.{Files, Path}

import org.apache.commons.io.FileUtils
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LauncherSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  var testDirectory: Path = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    testDirectory = Files.createTempDirectory(Path.of("."), "tmptest")
  }

  override def afterEach(): Unit = {
    super.afterEach()
    FileUtils.deleteDirectory(testDirectory.toFile)
  }

  "new command" should {
    "create a new project with correct structure" in {
      val projectDir = testDirectory.resolve("proj1")
      Launcher.newProject("TEST", Some(projectDir))
      projectDir.toFile should exist
      projectDir.resolve("src").resolve("Main.enso").toFile should exist
    }
  }

}
