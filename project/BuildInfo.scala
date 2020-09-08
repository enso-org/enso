import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object BuildInfo {

  /**
    * Writes build-time information to a Scala object that can be used by the
    * components.
    *
    * If the `ENSO_RELEASE_MODE` environment variable is set to `true`, will set
    * an `isRelease` flag to true. This flag can be used to disable
    * development-specific features.
    *
    * @param file location where to write the Scala code
    * @param log a logger instance for diagnostics
    * @param ensoVersion Enso version
    * @param scalacVersion Scala compiler version used in the project
    * @param graalVersion GraalVM version used in the project
    * @return sequence of modified files
    */
  def writeBuildInfoFile(
    file: File,
    log: ManagedLogger,
    ensoVersion: String,
    scalacVersion: String,
    graalVersion: String
  ): Seq[File] = {
    val gitInfo   = getGitInformation(log).getOrElse(fallbackGitInformation)
    val isRelease = isReleaseMode
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
         |
         |  // Release mode, set to true if the environment variable
         |  // `ENSO_RELEASE_MODE` is set to `true` at build time.
         |  val isRelease = $isRelease
         |}
         |""".stripMargin
    IO.write(file, fileContents)
    log.debug("Build info updated.")
    Seq(file)
  }

  private def isReleaseMode: Boolean =
    if (sys.env.get("ENSO_RELEASE_MODE").contains("true")) true else false

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
