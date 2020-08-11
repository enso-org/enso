package org.enso.launcher.components.runner

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.GlobalConfigurationManager
import org.enso.launcher.components.ComponentsManagerTest
import org.enso.launcher.installation.DistributionPaths
import org.enso.launcher.project.ProjectManager

class RunnerSpec extends ComponentsManagerTest {
  case class TestSetup(runner: Runner, paths: DistributionPaths)
  def makeFakeRunner(): Runner = {
    val (_, componentsManager) = makeManagers()

    val configurationManager = new GlobalConfigurationManager(componentsManager)
    val projectManager       = new ProjectManager(configurationManager)
    val runner =
      new Runner(projectManager, configurationManager, componentsManager)
    runner
  }

  "Runner" should {
    "create a command from settings" in {
      val runner = makeFakeRunner()

      val runSettings = RunSettings(SemVer(0, 0, 0), Seq("arg1", "--flag2"))
      val jvmOptions  = Seq(("locally-added-options", "value1"))
      val systemCommand = runner.createCommand(
        runSettings,
        JVMSettings(useSystemJVM = true, jvmOptions = jvmOptions)
      )

      systemCommand.command.head shouldEqual "java"

      val managedCommand = runner.createCommand(
        runSettings,
        JVMSettings(useSystemJVM = false, jvmOptions = jvmOptions)
      )

      managedCommand.command.head should include("java")
      managedCommand.extraEnv.find(_._1 == "JAVA_HOME").value._2 should
      include("graalvm-ce")

      for (command <- Seq(systemCommand, managedCommand)) {
        val commandLine = command.command.mkString(" ")
        commandLine should include("-Dlocally-added-options=value1")
        commandLine should include("-Doptions-added-from-manifest=42")
        commandLine should include("-Danother-one=true")
        commandLine should endWith("arg1 --flag2")

        commandLine should include
        regex("-Dtruffle.append.classpath=.*runtime.jar")

        commandLine should include
        regex("-jar .*runner.jar")
      }
    }
  }
}
