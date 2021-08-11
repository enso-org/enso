package org.enso.build
import java.nio.file.Path
import scala.sys.process._

/** Helper functions for working with the AWS CLI.
  *
  * These can be used on our CI. They assume that a profile called `s3-upload`
  * has been configured on the machine.
  */
object AWS {

  /** Name of the profile used for authentication. */
  val profile = "s3-upload"

  /** Default ACL settings for uploaded files. */
  val defaultACL = "public-read"

  /** Copies the file from source to the destination.
    *
    * Both source and destination can either be a path on the local filesystem
    * or an S3 URL.
    */
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

  /** An overload of [[transfer]] useful for uploading a file from a local path
    * to S3.
    */
  def transfer(source: Path, destination: String): Unit =
    transfer(source.toAbsolutePath.normalize.toString, destination)

  /** An overload of [[transfer]] useful for downloading a file from S3 to a
    * local path.
    */
  def transfer(source: String, destination: Path): Unit =
    transfer(source, destination.toAbsolutePath.normalize.toString)

  /** Deletes a file at a given path from the S3 storage. */
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

  /** Helper method to run the given bash command and check its exit code. */
  private def runCommand(command: String*): Unit = {
    val exitCode = command.!
    if (exitCode != 0) {
      throw new RuntimeException(
        s"Operation $command has failed with $exitCode exit code."
      )
    }
  }
}
