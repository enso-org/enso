package org.enso.launcher.project

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.{GlobalConfigurationManager, WithTemporaryDirectory}
import org.scalatest.TryValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ProjectManagerSpec
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with TryValues {
  private val defaultEnsoVersion = SemVer(0, 0, 0, Some("default"))
  def makeProjectManager(): ProjectManager = {
    val fakeConfigurationManager = new GlobalConfigurationManager(null) {
      override def defaultVersion: SemVer = defaultEnsoVersion
    }
    new ProjectManager(fakeConfigurationManager)
  }

  "ProjectManager" should {
    "create a new project with correct structure" in {
      val projectManager = makeProjectManager()
      val projectDir     = getTestDirectory.resolve("proj1")
      projectManager.newProject("Test Project", projectDir)

      projectDir.toFile should exist
      projectDir.resolve("src").resolve("Main.enso").toFile should exist

      val project = projectManager.loadProject(projectDir).success.value
      project.version shouldEqual defaultEnsoVersion
    }

    "find projects in parent directories" in {
      val projectManager = makeProjectManager()
      val projectDir     = getTestDirectory.resolve("proj1")
      projectManager.newProject("Test Project", projectDir)

      projectManager.findProject(projectDir) should be(defined)
      projectManager.findProject(projectDir.resolve("src")) should be(defined)
      projectManager.findProject(getTestDirectory) should be(empty)
    }
  }
}
