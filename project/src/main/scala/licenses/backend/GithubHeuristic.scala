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
      val homePage  = url(address).cat.!!
      val fileRegex = """<a .*? href="(.*?)".*?>(.*?)</a>""".r("href", "name")
      val matches = fileRegex
        .findAllMatchIn(homePage)
        .map(m => (m.group("name"), m.group("href")))
        .filter(p => mayBeRelevant(p._1))
        .toList
      matches.flatMap { case (_, href) =>
        try {
          val content =
            url("https://github.com" + href.replace("blob", "raw")).cat.!!
          Seq(
            AttachedFile(
              PortablePath.of(href),
              content,
              origin = Some("github.com")
            )
          )
        } catch {
          case NonFatal(error) =>
            log.warn(
              s"Found file $href but cannot download it: $error"
            )
            Seq()
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
