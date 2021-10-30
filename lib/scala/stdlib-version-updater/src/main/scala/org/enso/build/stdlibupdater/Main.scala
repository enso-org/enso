package org.enso.build.stdlibupdater

import cats.data.NonEmptyList
import org.enso.cli.arguments.{Application, Command, Opts}

import java.nio.file.Path
import scala.util.control.NonFatal
import cats.implicits._
import org.enso.cli.arguments.Opts.implicits._

object Main {
  private val commands: NonEmptyList[Command[Unit => Int]] = NonEmptyList.of(
    Command[Unit => Int](
      "check",
      "Checks if Standard Library versions are up to date."
    ) {
      Opts.pure { _ =>
        run(ReportingVisitor)
        0
      }
    },
    Command[Unit => Int]("update", "Updates Standard Library versions.") {
      val noFormat = Opts.flag(
        "no-format",
        "If set, does not run prettier after updating the packages.",
        showInUsage = true
      )

      noFormat map { disableFormatting => _ =>
        val shouldFormat = !disableFormatting
        run(new UpdatingVisitor(shouldFormat))
        0
      }
    }
  )

  private val application: Application[Unit] = Application(
    commandName = "sbt stdlib-version-updater/run",
    prettyName  = "Standard Library Version Updater",
    helpHeader  = "",
    commands    = commands
  )

  def run(visitor: StdlibVisitor): Unit = {
    val bundledLibRoot        = Path.of("distribution/lib")
    val targetVersion: String = buildinfo.Info.stdLibVersion

    val walker = new StdlibWalker(bundledLibRoot, targetVersion, visitor)
    walker.walk()
  }

  def main(args: Array[String]): Unit = try {
    application.run(args) match {
      case Left(value) =>
        value.foreach(println)
        sys.exit(1)
      case Right(exitCode) =>
        sys.exit(exitCode)
    }
  } catch {
    case NonFatal(error) =>
      println(s"Failed: $error")
      error.printStackTrace()
      sys.exit(1)
  }
}
