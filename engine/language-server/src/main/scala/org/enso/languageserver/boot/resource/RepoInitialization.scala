package org.enso.languageserver.boot.resource

import java.nio.file.{Files, NoSuchFileException}

import akka.event.EventStream
import org.enso.languageserver.data.DirectoriesConfig
import org.enso.languageserver.event.InitializedEvent
import org.enso.searcher.{FileVersionsRepo, SuggestionsRepo}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/** Initialization of the Language Server repositories.
  *
  * @param directoriesConfig configuration of language server directories
  * @param eventStream akka events stream
  * @param suggestionsRepo the suggestions repo
  * @param versionsRepo the versions repo
  */
class RepoInitialization(
  directoriesConfig: DirectoriesConfig,
  eventStream: EventStream,
  suggestionsRepo: SuggestionsRepo[Future],
  versionsRepo: FileVersionsRepo[Future]
)(implicit ec: ExecutionContext)
    extends InitializationComponent {

  private val log = LoggerFactory.getLogger(this.getClass)

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    for {
      _ <- suggestionsRepoInit
      _ <- versionsRepoInit
    } yield InitializationComponent.Initialized

  private def suggestionsRepoInit: Future[Unit] = {
    val initAction =
      for {
        _ <- Future {
          log.info("Initializing suggestions repo.")
        }
        _ <- suggestionsRepo.init.recoverWith { case NonFatal(error) =>
          recoverInitError(error)
        }
        _ <- Future {
          log.info("Initialized Suggestions repo.")
        }
      } yield ()
    initAction.onComplete {
      case Success(()) =>
        eventStream.publish(InitializedEvent.SuggestionsRepoInitialized)
      case Failure(ex) =>
        log.error("Failed to initialize SQL suggestions repo", ex)
    }
    initAction
  }

  private def versionsRepoInit: Future[Unit] = {
    val initAction =
      for {
        _ <- Future {
          log.info("Initializing versions repo.")
        }
        _ <- versionsRepo.init.recoverWith { case NonFatal(error) =>
          recoverInitError(error)
        }
        _ <- Future {
          log.info("Initialized Versions repo.")
        }
      } yield ()
    initAction.onComplete {
      case Success(()) =>
        eventStream.publish(InitializedEvent.FileVersionsRepoInitialized)
      case Failure(ex) =>
        log.error("Failed to initialize SQL versions repo", ex)
    }
    initAction
  }

  private def recoverInitError(error: Throwable): Future[Unit] =
    for {
      _ <- Future {
        log.warn(
          s"Failed to initialize the suggestions database " +
          s"${directoriesConfig.suggestionsDatabaseFile}. " +
          s"${error.getMessage}"
        )
      }
      _ <- clearDatabaseFile
      _ <- Future {
        log.info("Retrying database initialization.")
      }
      _ <- suggestionsRepo.init
    } yield ()

  private def clearDatabaseFile: Future[Unit] =
    Future {
      Files.delete(directoriesConfig.suggestionsDatabaseFile.toPath)
    }.recover { case _: NoSuchFileException =>
      log.warn(
        s"Failed to delete the database file. File does not exist " +
        s"${directoriesConfig.suggestionsDatabaseFile}"
      )
    }

}
