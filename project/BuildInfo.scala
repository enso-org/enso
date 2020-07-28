import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object BuildInfo {
  def writeBuildInfoFile(
    file: File,
    log: ManagedLogger,
    ensoVersion: String,
    scalacVersion: String,
    graalVersion: String
  ): Seq[File] = {
    val gitInfo = getGitInformation(log).getOrElse(fallbackGitInformation)
    val fileContents =
      s"""
         |package buildinfo
         |
         |object Info {
         |
         |  // Versions
         |  val ensoVersion   = "$ensoVersion"
         |  val scalacVersion = "$scalacVersion"
         |  val graalVersion  = "$graalVersion"
         |
         |  // Git Info
         |  val commit            = "${gitInfo.commit}"
         |  val ref               = "${gitInfo.ref}"
         |  val isDirty           = ${gitInfo.isDirty}
         |  val latestCommitDate  = "${gitInfo.latestCommitDate}"
         |}
         |""".stripMargin
    IO.write(file, fileContents)
    log.debug("Build info updated.")
    Seq(file)
  }

  private case class GitInformation(
    ref: String,
    commit: String,
    isDirty: Boolean,
    latestCommitDate: String
  )
  private def getGitInformation(log: ManagedLogger): Option[GitInformation] =
    try {
      val hash = ("git rev-parse HEAD" !!).trim
      val ref =
        try {
          val branchCommand = "git symbolic-ref -q --short HEAD"
          val tagCommand    = "git describe --tags --exact-match"
          val refCommand =
            branchCommand #|| tagCommand
          (refCommand !!).trim
        } catch {
          case e: Exception =>
            log.warn(
              "Cannot get name of git branch/tag, defaulting to \"HEAD\". " +
              s"(Caused by: $e)"
            )
            "HEAD"
        }
      val isDirty          = !("git status --porcelain" !!).trim.isEmpty
      val latestCommitDate = ("git log HEAD -1 --format=%cd" !!).trim
      Some(GitInformation(ref, hash, isDirty, latestCommitDate))
    } catch {
      case e: Exception =>
        log.warn(
          "Could not get any git information. The build will proceed but it " +
          s"will not contain the git metadata. (Caused by: $e)"
        )
        None
    }
  private def fallbackGitInformation: GitInformation =
    GitInformation(
      "<built outside of a git repository>",
      "<built outside of a git repository>",
      isDirty = false,
      "<built outside of a git repository>"
    )
}
