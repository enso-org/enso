package org.enso.launcher.components.runner

import nl.gn0s1s.bump.SemVer
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RunnerSpec extends AnyWordSpec with Matchers with OptionValues {
  def makeFakeRunner(): Runner = {
    ???
  }

  "Runner" should {
    "create a command from settings" in {
      val runner      = makeFakeRunner()
      val runSettings = RunSettings(SemVer(0, 1, 0), Seq("arg1", "--flag2"))
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

      managedCommand.command.head shouldEqual "TODO"
      managedCommand.extraEnv.find(_._1 == "JAVA_HOME").value shouldEqual "TODO"

      for (command <- Seq(systemCommand, managedCommand)) {
        val commandLine = command.command.mkString(" ")
        commandLine should include("-Dlocally-added-options=value1")
        commandLine should include("-Doptions-added-from-manifest=42")
        commandLine should include("-Danother-one=true")
        commandLine should include("-Dtruffle.append.classpath=Path to runtime")
        commandLine should include("Path to runner")
        commandLine should include("arg1 --flag2")
      }
    }
  }
}
