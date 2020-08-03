package org.enso.launcher.cli

import java.net.URI
import java.net.http.HttpRequest
import java.nio.file.Path

import org.enso.cli.{Application, Command, Opts, TopLevelBehavior}
import org.enso.cli.Opts.implicits._
import cats.implicits._
import org.enso.launcher.archive.Archive
import org.enso.launcher.releases.github.HTTPDownload

object DebugConsole {
  private val commands: Seq[Command[Unit => Unit]] = Seq(
    fetchCommand,
    downloadCommand,
    extractCommand,
    exitCommand
  )

  private val debugApplication = Application[Unit](
    "debug",
    "Debug Console",
    "internal debugging component",
    Opts.pure(() => TopLevelBehavior.Continue(())),
    commands
  )

  def run(): Unit = {
    while (true) {
      val args = Console.in.readLine().split(' ')
      debugApplication.run(args)
    }
  }

  private def exitCommand =
    Command[Unit => Unit]("exit", "Terminates the program", Seq("quit")) {
      Opts.optionalArgument[Int]("EXIT-CODE") map { exitCode => (_: Unit) =>
        sys.exit(exitCode.getOrElse(0))
      }
    }

  private def fetchCommand =
    Command[Unit => Unit]("fetch", "Makes a HTTP request") {
      Opts.positionalArgument[String]("URI") map { uri => (_: Unit) =>
        val req = HttpRequest.newBuilder(new URI(uri)).GET().build()
        val res = HTTPDownload.fetchString(req)
        println(res.waitForResult(true))
      }
    }

  private def downloadCommand =
    Command[Unit => Unit]("download", "Downloads a file through HTTP") {
      (
        Opts.positionalArgument[String]("URI"),
        Opts.positionalArgument[Path]("DESTINATION-PATH")
      ) mapN { (uri, destination) => (_: Unit) =>
        val req = HttpRequest.newBuilder(new URI(uri)).GET().build()
        val res = HTTPDownload.download(req, destination)
        println(res.waitForResult(true))
      }
    }

  private def extractCommand =
    Command[Unit => Unit]("extract", "Extracts an archive") {
      (
        Opts.positionalArgument[Path]("ARCHIVE-PATH"),
        Opts.positionalArgument[Path]("DESTINATION-PATH")
      ) mapN { (source, destination) => (_: Unit) =>
        val res = Archive.extractArchive(source, destination, None)
        println(res.waitForResult(true))
      }
    }
}
