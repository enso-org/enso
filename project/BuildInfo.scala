import sbt._

import scala.sys.process._

object BuildInfo {
  def writeBuildInfoFile(
    file: File,
    ensoVersion: String,
    scalacVersion: String,
    graalVersion: String
  ): Seq[File] = {
    val gitHash          = ("git rev-parse HEAD" !!).trim
    val gitBranch        = ("git rev-parse --abbrev-ref HEAD" !!).trim
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
         |  val branch            = "$gitBranch"
         |  val isDirty           = $isDirty
         |  val latestCommitDate  = "$latestCommitDate"
         |}
         |""".stripMargin
    IO.write(file, fileContents)
    Seq(file)
  }
}
