package src.main.scala.licenses.backend

import java.nio.file.Path

import sbt.Logger
import sbt.io.syntax.url
import src.main.scala.licenses.{
  AttachedFile,
  Attachment,
  DependencyInformation,
  PortablePath
}

import scala.sys.process._
import scala.util.control.NonFatal

/** Tries to find copyright mentions in the GitHub project homepage and any
  * copyright-related files contained in the repository root.
  *
  * This is a fallback backend that may be used if the primary backends do not
  * find anything. It should not be used otherwise, as the primary backends are
  * most likely to yield more precise results - for example, the
  * GitHub-published version may be different than the one we use.
  */
case class GithubHeuristic(info: DependencyInformation, log: Logger) {

  /** Runs the gathering process and returns any attachments found.
    *
    * It proceeds only if the project has an URL that seems to point to GitHub.
    */
  def run(): Seq[Attachment] = {
    info.url match {
      case Some(url) if url.contains("github.com") =>
        tryDownloadingAttachments(url.replace("http://", "https://"))
      case _ => Seq()
    }
  }

  /** Downloads the project homepage at `address` and looks for any copyright
    * mentions or links that may lead to copyright-related files.
    *
    * Any found files are fetched and saved into the results.
    */
  def tryDownloadingAttachments(address: String): Seq[Attachment] =
    try {
      val homePage    = url(address).cat.!!
      val branchRegex = """"defaultBranch":"([^"]*?)"""".r("branch")
      val branch      = branchRegex.findFirstMatchIn(homePage).map(_.group("branch"))
      branch match {
        case None =>
          log.warn(s"Cannot find default branch for $address")
          Seq()
        case Some(branch) =>
          val fileRegex =
            """\{"name":"([^"]*?)","path":"([^"]*?)","contentType":"file"\}"""
              .r("name", "path")
          val matches = fileRegex
            .findAllMatchIn(homePage)
            .map(m => (m.group("name"), m.group("path")))
            .filter(p => mayBeRelevant(p._1))
            .toList
          matches.flatMap { case (_, path) =>
            val rawHref = address + "/raw/" + branch + "/" + path
            // This path is reconstructed to match the 'legacy' format for compatibility with older versions of the review settings.
            // It has the format <org>/<repo>/blob/<branch>/<path>
            val internalPath = address
              .stripPrefix("https://github.com")
              .stripSuffix("/") + "/blob/" + branch + "/" + path
            try {
              val content = url(rawHref).cat.!!
              Seq(
                AttachedFile(
                  PortablePath.of(internalPath),
                  content,
                  origin = Some(address)
                )
              )
            } catch {
              case NonFatal(error) =>
                log.warn(
                  s"Found file $rawHref but cannot download it: $error"
                )
                Seq()
            }
          }
      }
    } catch {
      case NonFatal(error) =>
        log.warn(s"GitHub backend for ${info.packageName} failed with $error")
        Seq()
    }

  /** Decides if the file may be relevant and should be included in the result.
    *
    * Filenames that contain spaces are ignored because they are usually normal
    * links and do not lead to relevant files.
    */
  private def mayBeRelevant(name: String): Boolean = {
    val normalized = name.strip().toLowerCase()
    !normalized.contains(' ') &&
    GatherNotices.mayBeRelevant(normalized)
  }
}
