package org.enso.cli

import cats.data.NonEmptyList
import cats.implicits._
import org.enso.cli.arguments.Opts.implicits._
import org.enso.cli.arguments.{
  Application,
  Command,
  CommandHelp,
  Opts,
  PluginManager,
  TopLevelBehavior
}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{EitherValues, OptionValues}

class ApplicationSpec
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with EitherValues {
  "Application" should {
    "delegate to correct commands" in {
      var ranCommand: Option[String] = None
      val app = Application(
        "app",
        "App",
        "Test app.",
        NonEmptyList.of(
          Command("cmd1", "cmd1") {
            Opts.pure { _ =>
              ranCommand = Some("cmd1")
            }
          },
          Command("cmd2", "cmd2") {
            Opts.positionalArgument[String]("arg") map { arg => _ =>
              ranCommand = Some(arg)
            }
          }
        )
      )

      app.run(Seq("cmd1"))
      ranCommand.value shouldEqual "cmd1"

      app.run(Seq("cmd2", "argvalue"))
      ranCommand.value shouldEqual "argvalue"
    }

    "handle plugins" in {
      case class PluginRanException(name: String, args: Seq[String])
          extends RuntimeException

      val plugins = Seq("plugin1", "plugin2")
      val pluginManager = new PluginManager {
        override def runPlugin(name: String, args: Seq[String]): Nothing =
          if (plugins.contains(name))
            throw PluginRanException(name, args)
          else throw new RuntimeException("Plugin not found.")

        override def hasPlugin(name: String): Boolean =
          plugins.contains(name)

        override def pluginsNames(): Seq[String] = plugins

        override def pluginsHelp(): Seq[CommandHelp] =
          plugins.map(CommandHelp(_, ""))
      }
      val app = Application[Unit](
        "app",
        "App",
        "Test app.",
        Opts.pure[() => org.enso.cli.arguments.TopLevelBehavior[Unit]] { () =>
          TopLevelBehavior.Continue(())
        },
        NonEmptyList.of(
          Command[Unit => Unit]("cmd", "cmd") { Opts.pure { _ => () } }
        ),
        pluginManager
      )

      val pluginRun = intercept[PluginRanException] {
        app.run(Seq("plugin1", "arg1", "--flag"))
      }
      pluginRun.name shouldEqual "plugin1"
      pluginRun.args shouldEqual Seq("arg1", "--flag")

      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        app.run(Seq("--help"))
      }
      val output = stream.toString()
      output should include("plugin1")
      output should include("plugin2")
    }

    "handle top-level options (before and after the command)" in {
      var ranCommand: Option[String] = None
      val app = Application[String](
        "app",
        "App",
        "Test app.", {
          val halt = Opts.flag("halt", "halt", showInUsage = false)
          val setting =
            Opts.optionalParameter[String]("setting", "setting", "setting")
          (halt, setting) mapN { (halt, setting) => () =>
            if (halt)
              TopLevelBehavior.Halt
            else TopLevelBehavior.Continue(setting.getOrElse("none"))
          }
        },
        NonEmptyList.of(
          Command[String => Unit]("cmd1", "cmd1") {
            Opts.pure { setting =>
              ranCommand = Some(setting)
            }
          }
        )
      )

      assert(
        app.run(Seq("--halt", "cmd1")).isRight,
        "Should parse successfully."
      )
      ranCommand should not be defined

      assert(app.run(Seq("cmd1")).isRight, "Should parse successfully.")
      ranCommand.value shouldEqual "none"

      withClue("top-level option before command:") {
        ranCommand = None
        assert(
          app.run(Seq("--setting=SET", "cmd1")).isRight,
          "Should parse successfully."
        )
        ranCommand.value shouldEqual "SET"
      }

      withClue("top-level option after command:") {
        ranCommand = None
        assert(
          app.run(Seq("cmd1", "--setting=SET")).isRight,
          "Should parse successfully."
        )
        ranCommand.value shouldEqual "SET"
      }
    }
  }

  "support related commands" in {
    val app = Application(
      "app",
      "App",
      "Test app.",
      NonEmptyList.of(
        Command("cmd", "cmd", related = Seq("related")) { Opts.pure { _ => } }
      )
    )

    app.run(Seq("related")).left.value.head should include(
      "You may be looking for `app cmd`."
    )
  }

  "suggest similar commands on typo" in {
    var ranCommand: Option[String] = None
    val app = Application(
      "app",
      "App",
      "Test app.",
      NonEmptyList.of(
        Command("cmd1", "cmd1") {
          Opts.pure { _ =>
            ranCommand = Some("cmd1")
          }
        },
        Command("cmd2", "cmd2") {
          Opts.positionalArgument[String]("arg") map { arg => _ =>
            ranCommand = Some(arg)
          }
        }
      )
    )

    val error = app.run(Seq("cmd")).left.value.head
    error should include("cmd1")
    error should include("cmd2")
  }
}
