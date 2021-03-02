package org.enso.languageserver.boot

import akka.event.EventStream
import org.enso.languageserver.data.DirectoriesConfig
import org.enso.languageserver.event.InitializedEvent
import org.enso.searcher.{FileVersionsRepo, SuggestionsRepo}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Initialization of the Language Server resources. Creates the directories and
  * initializes the databases.
  *
  * @param eventStream system event stream
  * @param directoriesConfig configuration of directories that should be created
  * @param suggestionsRepo the suggestions repo
  * @param versionsRepo the file versions repo
  */
class ResourcesInitialization(
  eventStream: EventStream,
  directoriesConfig: DirectoriesConfig,
  suggestionsRepo: SuggestionsRepo[Future],
  versionsRepo: FileVersionsRepo[Future]
)(implicit ec: ExecutionContext)
    extends InitializationComponent {

  private val log = LoggerFactory.getLogger(this.getClass)

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] = {
    val directoriesInit: Future[Unit] = Future {
      directoriesConfig.createDirectories()
    }

    val initialization = for {
      _ <- directoriesInit
      _ <- Future.sequence(Seq(suggestionsRepoInit, versionsRepoInit))
    } yield InitializationComponent.Initialized

    initialization.onComplete {
      case Success(_) =>
        eventStream.publish(InitializedEvent.InitializationFinished)
      case _ =>
        eventStream.publish(InitializedEvent.InitializationFailed)
    }
    initialization
  }

  private def suggestionsRepoInit: Future[Unit] = {
    val initAction = suggestionsRepo.init
    initAction.onComplete {
      case Success(()) =>
        eventStream.publish(InitializedEvent.SuggestionsRepoInitialized)
      case Failure(ex) =>
        log.error("Failed to initialize SQL suggestions repo", ex)
    }
    initAction
  }

  private def versionsRepoInit: Future[Unit] = {
    val initAction = versionsRepo.init
    initAction.onComplete {
      case Success(()) =>
        eventStream.publish(InitializedEvent.FileVersionsRepoInitialized)
      case Failure(ex) =>
        log.error("Failed to initialize versions repo", ex)
    }
    initAction
  }

}
