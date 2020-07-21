package org.enso.cli

import cats.implicits._
import org.enso.cli.Opts.implicits._
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
        Seq(
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

    "handle top-level options" in {
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
        Seq(
          Command[String => Unit]("cmd1", "cmd1") {
            Opts.pure { setting =>
              ranCommand = Some(setting)
            }
          }
        )
      )

      app.run(Seq("--halt", "cmd1"))
      ranCommand should not be defined

      app.run(Seq("cmd1"))
      ranCommand.value shouldEqual "none"

      app.run(Seq("--setting=SET", "cmd1"))
      ranCommand.value shouldEqual "SET"
    }
  }

  /*"support related commands" in {

  }*/

  "suggest similar commands on typo" in {
    var ranCommand: Option[String] = None
    val app = Application(
      "app",
      "App",
      "Test app.",
      Seq(
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
