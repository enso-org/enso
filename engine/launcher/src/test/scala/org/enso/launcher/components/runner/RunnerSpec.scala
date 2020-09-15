package org.enso.launcher.components.runner

import java.nio.file.{Files, Path}
import java.util.UUID

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.Logger
import org.enso.launcher.components.ComponentsManagerTest
import org.enso.launcher.config.GlobalConfigurationManager
import org.enso.launcher.project.ProjectManager

class RunnerSpec extends ComponentsManagerTest {
  private val defaultEngineVersion = SemVer(0, 0, 0, Some("default"))

  def makeFakeRunner(
    cwdOverride: Option[Path]     = None,
    extraEnv: Map[String, String] = Map.empty
  ): Runner = {
    val (distributionManager, componentsManager, env) = makeManagers(extraEnv)
    val configurationManager =
      new GlobalConfigurationManager(componentsManager, distributionManager) {
        override def defaultVersion: SemVer = defaultEngineVersion
      }
    val projectManager = new ProjectManager(configurationManager)
    val cwd            = cwdOverride.getOrElse(getTestDirectory)
    val runner =
      new Runner(projectManager, configurationManager, componentsManager, env) {
        override protected val currentWorkingDirectory: Path = cwd
      }
    runner
  }

  "Runner" should {
    "create a command from settings" in {
      Logger.suppressWarnings {
        val envOptions = "-Xfrom-env -Denv=env"
        val runner =
          makeFakeRunner(extraEnv = Map("ENSO_JVM_OPTS" -> envOptions))

        val runSettings = RunSettings(SemVer(0, 0, 0), Seq("arg1", "--flag2"))
        val jvmOptions  = Seq(("locally-added-options", "value1"))

        val enginePath =
          getTestDirectory / "test_data" / "dist" / "0.0.0"
        val runtimePath =
          (enginePath / "component" / "runtime.jar").toAbsolutePath.normalize
        val runnerPath =
          (enginePath / "component" / "runner.jar").toAbsolutePath.normalize

        def checkCommandLine(command: Command): Unit = {
          val commandLine = command.command.mkString(" ")
          val arguments   = command.command.tail
          arguments should contain("-Xfrom-env")
          arguments should contain("-Denv=env")
          arguments should contain("-Dlocally-added-options=value1")
          arguments should contain("-Dlocally-added-options=value1")
          arguments should contain("-Doptions-added-from-manifest=42")
          arguments should contain("-Xanother-one")
          commandLine should endWith("arg1 --flag2")

          arguments should contain(s"-Dtruffle.class.path.append=$runtimePath")
          arguments.filter(
            _.contains("truffle.class.path.append")
          ) should have length 1

          commandLine should include(s"-jar $runnerPath")
        }

        runner.withCommand(
          runSettings,
          JVMSettings(useSystemJVM = true, jvmOptions = jvmOptions)
        ) { systemCommand =>
          systemCommand.command.head shouldEqual "java"
          checkCommandLine(systemCommand)
        }

        runner.withCommand(
          runSettings,
          JVMSettings(useSystemJVM = false, jvmOptions = jvmOptions)
        ) { managedCommand =>
          managedCommand.command.head should include("java")
          val javaHome =
            managedCommand.extraEnv.find(_._1 == "JAVA_HOME").value._2
          javaHome should include("graalvm-ce")
        }
      }
    }

    "create project with name, default author (if specified) and additional " +
    "arguments" in {
      val runner             = makeFakeRunner()
      val projectPath        = getTestDirectory / "project"
      val authorName         = "Author Name"
      val authorEmail        = "author@example.com"
      val additionalArgument = "additional arg"
      val runSettings = runner
        .newProject(
          path                = projectPath,
          name                = "ProjectName",
          version             = defaultEngineVersion,
          authorName          = Some(authorName),
          authorEmail         = Some(authorEmail),
          additionalArguments = Seq(additionalArgument)
        )
        .get

      runSettings.version shouldEqual defaultEngineVersion
      runSettings.runnerArguments should contain(additionalArgument)
      val commandLine = runSettings.runnerArguments.mkString(" ")
      commandLine should include(
        s"--new ${projectPath.toAbsolutePath.normalize}"
      )
      commandLine should include("--new-project-name ProjectName")
      commandLine should include(s"--new-project-author-name $authorName")
      commandLine should include(s"--new-project-author-email $authorEmail")
    }

    "run repl with default version and additional arguments" in {
      val runner = makeFakeRunner()
      val runSettings = runner
        .repl(
          projectPath         = None,
          versionOverride     = None,
          additionalArguments = Seq("arg", "--flag")
        )
        .get

      runSettings.version shouldEqual defaultEngineVersion
      runSettings.runnerArguments should (contain("arg") and contain("--flag"))
      runSettings.runnerArguments.mkString(" ") should
      (include("--repl") and not include (s"--in-project"))
    }

    "run repl in project context" in {
      val runnerOutside = makeFakeRunner()

      val version = SemVer(0, 0, 0, Some("repl-test"))
      version should not equal defaultEngineVersion // sanity check

      val projectPath    = getTestDirectory / "project"
      val normalizedPath = projectPath.toAbsolutePath.normalize.toString
      newProject("test", projectPath, version)

      val outsideProject = runnerOutside
        .repl(
          projectPath         = Some(projectPath),
          versionOverride     = None,
          additionalArguments = Seq()
        )
        .get

      outsideProject.version shouldEqual version
      outsideProject.runnerArguments.mkString(" ") should
      (include(s"--in-project $normalizedPath") and include("--repl"))

      val runnerInside = makeFakeRunner(Some(projectPath))
      val insideProject = runnerInside
        .repl(
          projectPath         = None,
          versionOverride     = None,
          additionalArguments = Seq()
        )
        .get

      insideProject.version shouldEqual version
      insideProject.runnerArguments.mkString(" ") should
      (include(s"--in-project $normalizedPath") and include("--repl"))

      val overridden = SemVer(0, 0, 0, Some("overridden"))
      val overriddenRun = runnerInside
        .repl(
          projectPath         = Some(projectPath),
          versionOverride     = Some(overridden),
          additionalArguments = Seq()
        )
        .get

      overriddenRun.version shouldEqual overridden
      overriddenRun.runnerArguments.mkString(" ") should
      (include(s"--in-project $normalizedPath") and include("--repl"))
    }

    "run language server" in {
      val runner = makeFakeRunner()

      val version     = SemVer(0, 0, 0, Some("language-server-test"))
      val projectPath = getTestDirectory / "project"
      newProject("test", projectPath, version)

      val options = LanguageServerOptions(
        rootId    = UUID.randomUUID(),
        path      = projectPath,
        interface = "127.0.0.2",
        rpcPort   = 1234,
        dataPort  = 4321
      )
      val runSettings = runner
        .languageServer(
          options,
          versionOverride     = None,
          additionalArguments = Seq("additional")
        )
        .get

      runSettings.version shouldEqual version
      val commandLine = runSettings.runnerArguments.mkString(" ")
      commandLine should include(s"--interface ${options.interface}")
      commandLine should include(s"--rpc-port ${options.rpcPort}")
      commandLine should include(s"--data-port ${options.dataPort}")
      commandLine should include(s"--root-id ${options.rootId}")
      val normalizedPath = options.path.toAbsolutePath.normalize.toString
      commandLine should include(s"--path $normalizedPath")
      runSettings.runnerArguments.lastOption.value shouldEqual "additional"

      val overridden = SemVer(0, 0, 0, Some("overridden"))
      runner
        .languageServer(
          options,
          versionOverride     = Some(overridden),
          additionalArguments = Seq()
        )
        .get
        .version shouldEqual overridden
    }

    "run a project" in {
      val runnerOutside = makeFakeRunner()

      val version        = SemVer(0, 0, 0, Some("run-test"))
      val projectPath    = getTestDirectory / "project"
      val normalizedPath = projectPath.toAbsolutePath.normalize.toString
      newProject("test", projectPath, version)

      val outsideProject = runnerOutside
        .run(
          path                = Some(projectPath),
          versionOverride     = None,
          additionalArguments = Seq()
        )
        .get

      outsideProject.version shouldEqual version
      outsideProject.runnerArguments.mkString(" ") should
      include(s"--run $normalizedPath")

      val runnerInside = makeFakeRunner(Some(projectPath))
      val insideProject = runnerInside
        .run(
          path                = None,
          versionOverride     = None,
          additionalArguments = Seq()
        )
        .get

      insideProject.version shouldEqual version
      insideProject.runnerArguments.mkString(" ") should
      include(s"--run $normalizedPath")

      val overridden = SemVer(0, 0, 0, Some("overridden"))
      val overriddenRun = runnerInside
        .run(
          path                = Some(projectPath),
          versionOverride     = Some(overridden),
          additionalArguments = Seq()
        )
        .get

      overriddenRun.version shouldEqual overridden
      overriddenRun.runnerArguments.mkString(" ") should
      include(s"--run $normalizedPath")

      assert(
        runnerOutside
          .run(
            path                = None,
            versionOverride     = None,
            additionalArguments = Seq()
          )
          .isFailure,
        "Running outside project without providing any paths should be an error"
      )
    }

    "run a script outside of a project even if cwd is inside project" in {
      val version     = SemVer(0, 0, 0, Some("run-test"))
      val projectPath = getTestDirectory / "project"
      val runnerInside =
        makeFakeRunner(cwdOverride = Some(projectPath))
      newProject("test", projectPath, version)

      val outsideFile    = getTestDirectory / "Main.enso"
      val normalizedPath = outsideFile.toAbsolutePath.normalize.toString
      Files.copy(
        projectPath / "src" / "Main.enso",
        outsideFile
      )

      val runSettings = runnerInside
        .run(
          path                = Some(outsideFile),
          versionOverride     = None,
          additionalArguments = Seq()
        )
        .get

      runSettings.version shouldEqual defaultEngineVersion
      runSettings.runnerArguments.mkString(" ") should
      (include(s"--run $normalizedPath") and (not(include("--in-project"))))
    }

    "run a script inside of a project" in {
      val version               = SemVer(0, 0, 0, Some("run-test"))
      val projectPath           = getTestDirectory / "project"
      val normalizedProjectPath = projectPath.toAbsolutePath.normalize.toString
      val runnerOutside         = makeFakeRunner()
      newProject("test", projectPath, version)

      val insideFile         = projectPath / "src" / "Main.enso"
      val normalizedFilePath = insideFile.toAbsolutePath.normalize.toString

      val runSettings = runnerOutside
        .run(
          path                = Some(insideFile),
          versionOverride     = None,
          additionalArguments = Seq()
        )
        .get

      runSettings.version shouldEqual version
      runSettings.runnerArguments.mkString(" ") should
      (include(s"--run $normalizedFilePath") and
      include(s"--in-project $normalizedProjectPath"))
    }

    "get default version outside of project" in {
      val runner = makeFakeRunner()
      val (runSettings, whichEngine) = runner
        .version(useJSON = true)
        .get

      runSettings.version shouldEqual defaultEngineVersion
      runSettings.runnerArguments should
      (contain("--version") and contain("--json"))

      whichEngine shouldEqual WhichEngine.Default
    }

    "get project version inside of project" in {
      val version      = SemVer(0, 0, 0, Some("version-test"))
      val projectPath  = getTestDirectory / "project"
      val name         = "Testname"
      val runnerInside = makeFakeRunner(cwdOverride = Some(projectPath))
      newProject(name, projectPath, version)
      val (runSettings, whichEngine) = runnerInside
        .version(useJSON = false)
        .get

      runSettings.version shouldEqual version
      runSettings.runnerArguments should
      (contain("--version") and not(contain("--json")))

      whichEngine shouldEqual WhichEngine.FromProject(name)
    }
  }
}
