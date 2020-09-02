package org.enso.cli

import cats.data.NonEmptyList
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
