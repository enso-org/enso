package src.main.scala.licenses.report

import java.security.MessageDigest

import sbt.{File, IO}
import src.main.scala.licenses.{DistributionDescription, ReviewedSummary}

import scala.util.control.NonFatal

case class ReviewState(hash: String, warningsCount: Int)

object ReviewState {
  def read(file: File): Option[ReviewState] = {
    try {
      val content            = IO.read(file)
      val Array(hash, count) = content.split(';')
      Some(ReviewState(hash, count.toInt))
    } catch { case NonFatal(_) => None }
  }

  def write(file: File, reviewState: ReviewState): Unit = {
    IO.createDirectory(file.getParentFile)
    IO.write(file, s"${reviewState.hash};${reviewState.warningsCount}")
  }

  def computeHash(distributionDescription: DistributionDescription): String =
    distributionDescription match {
      case DistributionDescription(
            artifactName,
            packageDestination,
            sbtComponents
          ) =>
        val digest = MessageDigest.getInstance("SHA-256")
        digest.update(artifactName.getBytes)
        digest.update(packageDestination.toString.getBytes)
        for (sbtComponent <- sbtComponents) {
          digest.update(sbtComponent.name.getBytes)
          val dependencies =
            sbtComponent.licenseReport.licenses.sortBy(_.module.toString)
          for (dep <- dependencies) {
            digest.update(dep.module.toString.getBytes)
            digest.update(dep.license.name.getBytes)
          }
        }
        digest.digest().map("%02X".format(_)).mkString
    }
}
