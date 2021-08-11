package org.enso.build
import java.nio.file.Path
import scala.sys.process._

object AWS {
  val profile    = "s3-upload"
  val defaultACL = "public-read"

  def runCommand(command: String*): Unit = {
    val exitCode = command.!
    if (exitCode != 0) {
      throw new RuntimeException(
        s"Operation $command has failed with $exitCode exit code."
      )
    }
  }

  def transfer(source: Path, destination: String): Unit =
    transfer(source.toAbsolutePath.normalize.toString, destination)

  def transfer(source: String, destination: Path): Unit =
    transfer(source, destination.toAbsolutePath.normalize.toString)

  def transfer(source: String, destination: String): Unit = {
    runCommand(
      "aws",
      "s3",
      "cp",
      source,
      destination,
      "--profile",
      profile,
      "--acl",
      defaultACL
    )
  }

  def delete(path: String): Unit = {
    runCommand(
      "aws",
      "s3",
      "rm",
      path,
      "--profile",
      profile
    )
  }
}
