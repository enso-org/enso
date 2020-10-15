package src.main.scala.licenses.report

import java.nio.file.Files
import java.security.MessageDigest

import sbt.internal.util.ManagedLogger
import sbt.{File, IO}
import src.main.scala.licenses.{
  DistributionDescription,
  FilesHelper,
  PortablePath
}

import scala.util.control.NonFatal

case class ReviewState(
  inputHash: String,
  outputHash: String,
  warningsCount: Int
)

object ReviewState {
  def read(file: File, log: ManagedLogger): Option[ReviewState] = {
    try {
      IO.readLines(file) match {
        case List(inputHash, outputHash, count) =>
          Some(ReviewState(inputHash, outputHash, count.toInt))
        case _ =>
          log.error(s"Review state at $file is malformed.")
          None
      }
    } catch {
      case NonFatal(error) =>
        log.error(s"Could not read review state at $file: $error")
        None
    }
  }

  def write(file: File, reviewState: ReviewState): Unit = {
    IO.createDirectory(file.getParentFile)
    IO.write(
      file,
      Seq(
        reviewState.inputHash,
        reviewState.outputHash,
        reviewState.warningsCount
      ).mkString("", "\n", "\n")
    )
  }

  def computeInputHash(
    distributionDescription: DistributionDescription
  ): String = {
    val DistributionDescription(
      artifactName,
      _,
      sbtComponents
    ) = distributionDescription

    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(artifactName.getBytes)
    for (sbtComponent <- sbtComponents) {
      digest.update(sbtComponent.name.getBytes)
      val dependencies =
        sbtComponent.licenseReport.licenses.sortBy(_.module.toString)
      for (dep <- dependencies) {
        digest.update(dep.module.toString.getBytes)
        digest.update(dep.license.name.getBytes)
      }
    }
    hexString(digest.digest())
  }

  def computeOutputHash(
    distributionDescription: DistributionDescription
  ): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val root   = distributionDescription.packageDestination.toPath
    val allFiles =
      FilesHelper
        .walk(root)(Seq(_))
        .map(p => PortablePath(root.relativize(p)))
        .sortBy(_.toString)
    for (path <- allFiles) {
      digest.update(path.toString.getBytes)
      val file = root.resolve(path.path).toFile
      if (!file.isDirectory) {
        digest.update(IO.readBytes(file))
      }
    }
    hexString(digest.digest())
  }

  private def hexString(bytes: Array[Byte]): String =
    bytes.map("%02X".format(_)).mkString

  def write(
    file: File,
    distributionDescription: DistributionDescription,
    warningsCount: Int
  ): Unit = {
    val state = ReviewState(
      inputHash     = computeInputHash(distributionDescription),
      outputHash    = computeOutputHash(distributionDescription),
      warningsCount = warningsCount
    )
    write(file, state)
  }
}
