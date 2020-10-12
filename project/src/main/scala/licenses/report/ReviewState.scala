package src.main.scala.licenses.report

import java.nio.file.Files
import java.security.MessageDigest

import sbt.{File, IO}
import src.main.scala.licenses.{DistributionDescription, FilesHelper}

import scala.util.control.NonFatal

case class ReviewState(
  inputHash: String,
  outputHash: String,
  warningsCount: Int
)

object ReviewState {
  def read(file: File): Option[ReviewState] = {
    try {
      val content                             = IO.read(file)
      val Array(inputHash, outputHash, count) = content.strip().split(';')
      Some(ReviewState(inputHash, outputHash, count.toInt))
    } catch { case NonFatal(_) => None }
  }

  def write(file: File, reviewState: ReviewState): Unit = {
    IO.createDirectory(file.getParentFile)
    IO.write(
      file,
      s"${reviewState.inputHash};${reviewState.outputHash};" +
      s"${reviewState.warningsCount}\n"
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
      FilesHelper.walk(root)(Seq(_)).sortBy(p => root.relativize(p).toString)
    for (path <- allFiles) {
      if (!Files.isDirectory(path)) {
        digest.update(IO.readBytes(path.toFile))
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
