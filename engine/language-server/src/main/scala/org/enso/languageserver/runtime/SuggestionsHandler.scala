package org.enso.languageserver.runtime

import java.nio.file.Path

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import org.enso.languageserver.data.Config
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.languageserver.runtime.SearchProtocol._
import org.enso.languageserver.util.UnhandledLogging
import org.enso.pkg.PackageManager
import org.enso.polyglot.Suggestion
import org.enso.searcher.{SuggestionEntry, SuggestionsRepo}
import org.enso.text.editing.model.Position

import scala.concurrent.Future
import scala.util.Try

/**
  * The handler of search requests.
  *
  * Handler initializes the database and responds to the search requests.
  *
  * @param config the server configuration
  * @param repo the suggestions repo
  */
final class SuggestionsHandler(config: Config, repo: SuggestionsRepo[Future])
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ProjectNameChangedEvent])
    config.contentRoots.foreach {
      case (_, contentRoot) =>
        PackageManager.Default
          .fromDirectory(contentRoot)
          .foreach(pkg => self ! ProjectNameChangedEvent(pkg.config.name))
    }
  }

  override def receive: Receive = {
    case ProjectNameChangedEvent(name) =>
      context.become(initialized(name))
    case _ =>
      sender() ! ProjectNotFoundError
  }
  def initialized(projectName: String): Receive = {
    case GetSuggestionsDatabaseVersion =>
      repo.currentVersion
        .map(GetSuggestionsDatabaseVersionResult)
        .pipeTo(sender())

    case GetSuggestionsDatabase =>
      repo.getAll
        .map(Function.tupled(toGetSuggestionsDatabaseResult))
        .pipeTo(sender())

    case Completion(path, pos, selfType, returnType, tags) =>
      val kinds = tags.map(_.map(SuggestionKind.toSuggestion))
      val module = for {
        rootFile <-
          config.findContentRoot(path.rootId).left.map(FileSystemError)
        module <-
          getModule(projectName, rootFile.toPath, path.toFile(rootFile).toPath)
            .toRight(ModuleNotResolvedError(path))
      } yield module

      module
        .fold(
          Future.successful,
          module => {
            repo
              .search(
                Some(module),
                selfType,
                returnType,
                kinds,
                Some(toPosition(pos))
              )
              .map(CompletionResult.tupled)
          }
        )
        .pipeTo(sender())

    case ProjectNameChangedEvent(name) =>
      context.become(initialized(name))
  }

  private def toGetSuggestionsDatabaseResult(
    version: Long,
    entries: Seq[SuggestionEntry]
  ): GetSuggestionsDatabaseResult = {
    val updates = entries.map(entry =>
      SuggestionsDatabaseUpdate.Add(entry.id, entry.suggestion)
    )
    GetSuggestionsDatabaseResult(updates, version)
  }

  private def getModule(
    projectName: String,
    root: Path,
    file: Path
  ): Option[String] = {
    getModuleSegments(root, file).map { modules =>
      toModule(projectName +: modules :+ getModuleName(file))
    }
  }

  private def getModuleSegments(
    root: Path,
    file: Path
  ): Option[Vector[String]] = {
    Try(root.relativize(file)).toOption
      .map { relativePath =>
        val b = Vector.newBuilder[String]
        1.until(relativePath.getNameCount - 1)
          .foreach(i => b += relativePath.getName(i).toString)
        b.result()
      }
  }

  private def getModuleName(path: Path): String = {
    val fileName = path.getFileName.toString
    fileName.substring(0, fileName.lastIndexOf('.'))
  }

  private def toModule(segments: Iterable[String]): String =
    segments.mkString(".")

  private def toPosition(pos: Position): Suggestion.Position =
    Suggestion.Position(pos.line, pos.character)
}

object SuggestionsHandler {

  /**
    * Creates a configuration object used to create a [[SuggestionsHandler]].
    *
    * @param config the server configuration
    * @param repo the suggestions repo
    */
  def props(config: Config, repo: SuggestionsRepo[Future]): Props =
    Props(new SuggestionsHandler(config, repo))

}
