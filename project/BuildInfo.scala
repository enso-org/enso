import sbt._

import scala.sys.process._

object BuildInfo {
  def writeBuildInfoFile(
    file: File,
    ensoVersion: String,
    scalacVersion: String,
    graalVersion: String
  ): Seq[File] = {
    val gitHash            = ("git rev-parse HEAD" !!).trim
    val gitBranchCommand   = "git symbolic-ref -q --short HEAD"
    val gitTagCommand      = "git describe --tags --exact-match"
    val gitFallbackCommand = "git rev-parse --abbrev-ref HEAD"
    val gitRefCommand =
      gitBranchCommand #|| gitTagCommand #|| gitFallbackCommand
    val gitRef           = (gitRefCommand !!).trim
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
    Seq(file)
  }
}
