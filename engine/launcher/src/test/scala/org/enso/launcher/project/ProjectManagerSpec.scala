package org.enso.launcher.project

import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.config.GlobalConfigurationManager
import org.enso.runtimeversionmanager.distribution.DistributionManager
import org.enso.runtimeversionmanager.test.RuntimeVersionManagerTest
import org.enso.pkg.Contact
import org.scalatest.{Inside, OptionValues}

class ProjectManagerSpec
    extends RuntimeVersionManagerTest
    with Inside
    with OptionValues {
  private val defaultEnsoVersion = SemVer(0, 0, 0, Some("default"))
  def makeProjectManager(): (GlobalConfigurationManager, ProjectManager) = {
    val env                 = fakeInstalledEnvironment()
    val distributionManager = new DistributionManager(env)
    val fakeConfigurationManager =
      new GlobalConfigurationManager(null, distributionManager) {
        override def defaultVersion: SemVer = defaultEnsoVersion
      }
    (fakeConfigurationManager, new ProjectManager(fakeConfigurationManager))
  }

  "ProjectManager" should {
    "create a new project with correct structure" ignore {
      val (configManager, projectManager) = makeProjectManager()

      val author = Contact(Some("author"), Some("a@example.com"))
      configManager.updateConfig(
        _.copy(
          authorName  = author.name,
          authorEmail = author.email
        )
      )

      val projectDir = getTestDirectory.resolve("proj1")

      // TODO [RW] create new project using runner.jar (to be added with #1046)
      // this test may then be moved to the `runner` package

      projectDir.toFile should exist
      projectDir.resolve("src").resolve("Main.enso").toFile should exist

      val project = projectManager.loadProject(projectDir).get
      project.version shouldEqual defaultEnsoVersion
      project.config.authors.headOption.value shouldEqual author
      project.config.maintainers.headOption.value shouldEqual author
    }

    "find projects in parent directories" in {
      val (_, projectManager) = makeProjectManager()
      val projectDir          = getTestDirectory.resolve("proj1")
      newProject("Test Project", projectDir, defaultEnsoVersion)

      projectManager.findProject(projectDir).get should be(defined)
      projectManager.findProject(projectDir.resolve("src")).get should
      be(defined)

      projectManager.findProject(getTestDirectory).get should be(empty)
    }
  }
}
