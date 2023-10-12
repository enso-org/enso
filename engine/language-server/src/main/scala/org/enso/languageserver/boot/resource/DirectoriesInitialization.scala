package org.enso.languageserver.boot.resource

import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.ProjectDirectoriesConfig

import scala.concurrent.{ExecutionContext, Future}

/** Directories initialization.
  *
  * @param directoriesConfig the directories config
  */
final class DirectoriesInitialization(
  directoriesConfig: ProjectDirectoriesConfig
)(implicit
  ec: ExecutionContext
) extends InitializationComponent
    with LazyLogging {

  @volatile
  private var _isInitialized: Boolean = false

  /** @inheritdoc */
  override def isInitialized: Boolean = _isInitialized

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] = {
    if (isInitialized) Future.successful(InitializationComponent.Initialized)
    else
      Future {
        logger.info("Initializing directories...")
        directoriesConfig.createDirectories()
        logger.info("Initialized directories.")
        _isInitialized = true
        InitializationComponent.Initialized
      }
  }
}
