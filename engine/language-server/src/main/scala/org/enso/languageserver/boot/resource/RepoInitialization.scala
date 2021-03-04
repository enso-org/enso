package org.enso.languageserver.boot.resource

import akka.event.EventStream
import org.enso.languageserver.event.InitializedEvent
import org.enso.searcher.{FileVersionsRepo, SuggestionsRepo}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Initialization of the Language Server repositories.
  *
  * @param eventStream akka events stream
  * @param suggestionsRepo the suggestions repo
  * @param versionsRepo the versions repo
  */
class RepoInitialization(
  eventStream: EventStream,
  suggestionsRepo: SuggestionsRepo[Future],
  versionsRepo: FileVersionsRepo[Future]
)(implicit ec: ExecutionContext)
    extends InitializationComponent {

  private val log = LoggerFactory.getLogger(this.getClass)

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    for {
      _ <- Future.sequence(Seq(suggestionsRepoInit, versionsRepoInit))
    } yield InitializationComponent.Initialized

  private def suggestionsRepoInit: Future[Unit] = {
    val initAction = suggestionsRepo.init
    initAction.onComplete {
      case Success(()) =>
        eventStream.publish(InitializedEvent.SuggestionsRepoInitialized)
      case Failure(ex) =>
        log.error("Failed to initialize SQL suggestions repo", ex)
    }
    log.info("Initialized Suggestions repo.")
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
    log.info("Initialized Versions repo.")
    initAction
  }

}
