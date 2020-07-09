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
    val gitHash          = ("git rev-parse HEAD" !!).trim
    val gitBranchCommand = "git symbolic-ref -q --short HEAD"
    val gitTagCommand    = "git describe --tags --exact-match"
    val gitRefCommand =
      gitBranchCommand #|| gitTagCommand
    val gitRef =
      try { (gitRefCommand !!).trim }
      catch {
        case e: Exception =>
          log.warn(
            "Cannot get name of git branch/tag, defaulting to \"HEAD\". " +
            s"(Caused by: ${e.getMessage})"
          )
          "HEAD"
      }
    val isDirty          = !("git status --porcelain" !!).trim.isEmpty
    val latestCommitDate = ("git log HEAD -1 --format=%cd" !!).trim

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
         |  val commit            = "$gitHash"
         |  val ref               = "$gitRef"
         |  val isDirty           = $isDirty
         |  val latestCommitDate  = "$latestCommitDate"
         |}
         |""".stripMargin
    IO.write(file, fileContents)
    log.debug("Build info updated.")
    Seq(file)
  }
}
