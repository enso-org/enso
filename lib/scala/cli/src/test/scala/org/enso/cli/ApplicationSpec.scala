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

  private def captureOutput(thunk: => Unit): String = {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream)(thunk)
    stream.toString()
  }

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
              0
            }
          },
          Command("cmd2", "cmd2") {
            Opts.positionalArgument[String]("arg") map { arg => _ =>
              ranCommand = Some(arg)
              0
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
        override def runPlugin(name: String, args: Seq[String]): Int =
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
          Command[Unit => Int]("cmd", "cmd") {
            Opts.pure { _ => 0 }
          }
        ),
        pluginManager
      )

      val pluginRun = intercept[PluginRanException] {
        app.run(Seq("plugin1", "arg1", "--flag"))
      }
      pluginRun.name shouldEqual "plugin1"
      pluginRun.args shouldEqual Seq("arg1", "--flag")

      val output = captureOutput {
        app.run(Seq("--help"))
      }
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
              TopLevelBehavior.Halt(10)
            else TopLevelBehavior.Continue(setting.getOrElse("none"))
          }
        },
        NonEmptyList.of(
          Command[String => Int]("cmd1", "cmd1") {
            Opts.pure { setting =>
              ranCommand = Some(setting)
              42
            }
          }
        )
      )

      app.run(Seq("--halt", "cmd1")) shouldEqual Right(10)
      app.run(Seq("cmd1", "--halt")) shouldEqual Right(10)

      ranCommand should not be defined

      app.run(Seq("cmd1")) shouldEqual Right(42)
      ranCommand.value shouldEqual "none"

      withClue("top-level option before command:") {
        ranCommand = None
        app.run(Seq("--setting=SET", "cmd1")) shouldEqual Right(42)
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

    "support related commands" in {
      val app = Application(
        "app",
        "App",
        "Test app.",
        NonEmptyList.of(
          Command("cmd", "cmd", related = Seq("related")) {
            Opts.pure { _ => 0 }
          }
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
              0
            }
          },
          Command("cmd2", "cmd2") {
            Opts.positionalArgument[String]("arg") map { arg => _ =>
              ranCommand = Some(arg)
              0
            }
          }
        )
      )

      val error = app.run(Seq("cmd")).left.value.head
      error should include("cmd1")
      error should include("cmd2")
    }

    def appWithSubcommands(): Application[_] = {
      val sub1 = Command[Boolean => Int]("sub1", "Sub1.") {
        val flag = Opts.flag("inner-flag", "Inner.", showInUsage = true)
        flag map { _ => _ => 0 }
      }
      val sub2 = Command[Boolean => Int]("sub2", "Sub2.") {
        val arg = Opts.optionalArgument[String]("ARG")
        arg map { _ => _ => 0 }
      }
      val topLevelOpts =
        Opts.flag("toplevel-flag", "Top.", showInUsage = true) map {
          flag => () =>
            TopLevelBehavior.Continue(flag)
        }
      val app = Application(
        "app",
        "App",
        "Top Header",
        topLevelOpts,
        NonEmptyList.of(
          Command("cmd", "Cmd.")(Opts.subcommands(sub1, sub2))
        )
      )

      app
    }

    "handle errors nicely" in {
      val app = appWithSubcommands()

      def runErrors(args: String*): String =
        CLIOutput.alignAndWrap(app.run(args).left.value.mkString("\n"))

      withClue("no commands reports it and displays help") {
        runErrors() should (include(
          "Expected a command."
        ) and include("Top Header"))
      }

      withClue("show similar commands if available") {
        runErrors("cmd1").replace("\r\n", "\n") should (include(
          "`cmd1` is not a valid command."
        ) and include(
          """The most similar commands are
            |    cmd
            |""".replace("\r\n", "\n").stripMargin
        ))
      }

      withClue("show available commands if no similar available") {
        runErrors("very-strange-command-name").replace(
          "\r\n",
          "\n"
        ) should (include(
          "`very-strange-command-name` is not a valid command."
            .replace("\r\n", "\n")
        ) and include("""Available commands:
                        |    cmd Cmd.
                        |""".replace("\r\n", "\n").stripMargin))
      }

      withClue("show command help if subcommand is missing") {
        runErrors("cmd") should (include("Expected a subcommand.") and include(
          "Cmd."
        ))
      }

      withClue("show similar subcommands if available") {
        runErrors("cmd", "sub").replace("\r\n", "\n") should (include(
          "is not a valid subcommand.".replace("\r\n", "\n")
        ) and include(
          """The most similar subcommands are
            |    sub1
            |    sub2
            |""".replace("\r\n", "\n").stripMargin
        ))
      }

      withClue("show available subcommands if no similar ones") {
        runErrors("cmd", "very-strange-subcommand").replace(
          "\r\n",
          "\n"
        ) should (include(
          "is not a valid subcommand."
        ) and include(
          """Available subcommands are
            |    sub1
            |    sub2
            |""".replace("\r\n", "\n").stripMargin
        ))
      }
    }

    "correctly handle help for subcommands, including top-level options" in {
      val app = appWithSubcommands()

      val cmdOutput = captureOutput {
        assert(app.run(Seq("cmd", "--help")).isRight)
      }.replace("\r\n", "\n")
      val cmdHelp =
        """Cmd.
          |Usage: app cmd sub1 [options] [--inner-flag]
          |               Sub1.
          |       app cmd sub2 [options] [ARG]
          |               Sub2.
          |
          |Available options:
          |    [--inner-flag]    Inner.
          |    [--toplevel-flag] Top.
          |    [-h | --help]     Print this help message.
          |""".replace("\r\n", "\n").stripMargin
      cmdOutput shouldEqual cmdHelp

      val topOutput = captureOutput {
        assert(app.run(Seq("--help")).isRight)
      }
      val topHelp =
        """Top Header
          |Usage: app [--toplevel-flag] [--help] COMMAND [ARGS...]
          |
          |Available commands:
          |    cmd Cmd.
          |
          |Available options:
          |    [--toplevel-flag] Top.
          |    [-h | --help]     Print this help message.
          |
          |For more information on a specific command listed above, please run `app COMMAND
          |--help`.
          |""".stripMargin

      topOutput.replace("\r\n", "\n") shouldEqual topHelp.replace("\r\n", "\n")
    }
  }
}
