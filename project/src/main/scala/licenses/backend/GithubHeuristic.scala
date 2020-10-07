package src.main.scala.licenses.backend

import java.nio.file.Path

import sbt.Logger
import sbt.io.syntax.url

import scala.sys.process._
import src.main.scala.licenses.{
  AttachedFile,
  Attachment,
  CopyrightMention,
  DependencyInformation
}

import scala.util.control.NonFatal

case class GithubHeuristic(info: DependencyInformation, log: Logger) {
  def run(): Seq[Attachment] = {
    info.url match {
      case Some(url) if url.contains("github.com") =>
        tryDownloadingAttachments(url.replace("http://", "https://"))
      case _ => Seq()
    }
  }

  def tryDownloadingAttachments(address: String): Seq[Attachment] =
    try {
      val homePage  = url(address).cat.!!
      val fileRegex = """<a .*? href="(.*?)".*?>(.*?)</a>""".r("href", "name")
      val matches = fileRegex
        .findAllMatchIn(homePage)
        .map(m => (m.group("name"), m.group("href")))
        .filter(p => mayBeNotice(p._1))
        .toList
      val files = matches.flatMap {
        case (_, href) =>
          try {
            val content =
              url("https://github.com" + href.replace("blob", "raw")).cat.!!
            Seq(
              AttachedFile(Path.of(href), content, origin = Some("github.com"))
            )
          } catch {
            case NonFatal(error) =>
              log.warn(
                s"Found file $href but cannot download it: $error"
              )
              Seq()
          }
      }
      val copyrights = homePage.linesIterator.toList
        .filter(_.toLowerCase.contains("copyright"))
        .map(line =>
          CopyrightMention(
            line,
            Seq(s"Found at $address"),
            Seq(Path.of("github.com"))
          )
        )
      files ++ copyrights
    } catch {
      case NonFatal(error) =>
        log.warn(s"GitHub backend for ${info.packageName} failed with $error")
        Seq()
    }

  private def mayBeNotice(name: String): Boolean = {
    val normalized = name.strip().toLowerCase()
    !normalized.contains(' ') &&
    GatherNotices.possibleNames.exists(normalized.contains)
  }
}
