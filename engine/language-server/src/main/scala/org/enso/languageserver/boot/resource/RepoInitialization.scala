package org.enso.languageserver.boot.resource

import java.io.IOException
import java.nio.file.{FileSystemException, Files, NoSuchFileException}

import akka.event.EventStream
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.enso.languageserver.data.ProjectDirectoriesConfig
import org.enso.languageserver.event.InitializedEvent
import org.enso.logger.masking.MaskedPath
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/** Initialization of the Language Server repositories.
  *
  * @param directoriesConfig configuration of language server directories
  * @param eventStream akka events stream
  * @param sqlDatabase the sql database
  * @param suggestionsRepo the suggestions repo
  */
class RepoInitialization(
  directoriesConfig: ProjectDirectoriesConfig,
  eventStream: EventStream,
  sqlDatabase: SqlDatabase,
  suggestionsRepo: SqlSuggestionsRepo
)(implicit ec: ExecutionContext)
    extends InitializationComponent
    with LazyLogging {

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    for {
      _ <- sqlDatabaseInit
      _ <- suggestionsRepoInit
    } yield InitializationComponent.Initialized

  private def sqlDatabaseInit: Future[Unit] = {
    val initAction = Future {
      logger.info("Initializing sql database [{}]...", sqlDatabase)
      sqlDatabase.open()
      logger.info("Initialized sql database [{}].", sqlDatabase)
    }
    initAction.onComplete {
      case Success(()) =>
      case Failure(ex) =>
        logger.error("Failed to initialize sql database [{}].", sqlDatabase, ex)
    }
    initAction
  }

  private def suggestionsRepoInit: Future[Unit] = {
    val initAction =
      for {
        _ <- Future {
          logger.info(
            "Initializing suggestions repo [{}]...",
            MaskedPath(directoriesConfig.suggestionsDatabaseFile.toPath)
          )
        }
        _ <- suggestionsRepo.init.recoverWith { case NonFatal(error) =>
          recoverInitError(error, suggestionsRepo.db)
        }
        _ <- Future {
          logger.info(
            "Initialized Suggestions repo [{}].",
            MaskedPath(directoriesConfig.suggestionsDatabaseFile.toPath)
          )
        }
      } yield ()
    initAction.onComplete {
      case Success(()) =>
        eventStream.publish(InitializedEvent.SuggestionsRepoInitialized)
      case Failure(ex) =>
        logger.error(
          "Failed to initialize SQL suggestions repo [{}].",
          MaskedPath(directoriesConfig.suggestionsDatabaseFile.toPath),
          ex
        )
    }
    initAction
  }

  private def recoverInitError(
    error: Throwable,
    db: SqlDatabase
  ): Future[Unit] =
    for {
      _ <- Future {
        logger.warn(
          "Failed to initialize the suggestions database [{}].",
          MaskedPath(directoriesConfig.suggestionsDatabaseFile.toPath),
          error
        )
      }
      _ <- Future(db.close())
      _ <- clearDatabaseFile()
      _ <- Future(db.open())
      _ <- Future {
        logger.info("Retrying database initialization.")
      }
      _ <- suggestionsRepo.init
    } yield ()

  private def clearDatabaseFile(retries: Int = 0): Future[Unit] = {
    Future {
      logger.info("Clear database file. Attempt #{}.", retries + 1)
      Files.delete(directoriesConfig.suggestionsDatabaseFile.toPath)
    }.recoverWith {
      case _: NoSuchFileException =>
        logger.warn(
          "Failed to delete the database file. Attempt #{}. " +
          "File does not exist [{}].",
          retries + 1,
          MaskedPath(directoriesConfig.suggestionsDatabaseFile.toPath)
        )
        Future.successful(())
      case error: FileSystemException =>
        logger.error(
          "Failed to delete the database file. Attempt #{}. " +
          "The file will be removed during the shutdown.",
          retries + 1,
          error
        )
        sys.addShutdownHook(
          FileUtils.deleteQuietly(directoriesConfig.suggestionsDatabaseFile)
        )
        Future.failed(error)
      case error: IOException =>
        logger.error(
          "Failed to delete the database file. Attempt #{}.",
          retries + 1,
          error
        )
        if (retries < RepoInitialization.MaxRetries) {
          Thread.sleep(1000)
          clearDatabaseFile(retries + 1)
        } else {
          Future.failed(error)
        }
    }
  }

}

object RepoInitialization {

  val MaxRetries = 3
}
