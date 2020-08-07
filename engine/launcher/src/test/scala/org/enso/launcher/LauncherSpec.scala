package org.enso.launcher

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LauncherSpec
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory {

  "new command" should {
    "create a new project with correct structure" in {
      Logger.suppressWarnings {
        val projectDir = getTestDirectory.resolve("proj1")
        Launcher.newProject("TEST", Some(projectDir))
        projectDir.toFile should exist
        projectDir.resolve("src").resolve("Main.enso").toFile should exist
      }
    }
  }

}
