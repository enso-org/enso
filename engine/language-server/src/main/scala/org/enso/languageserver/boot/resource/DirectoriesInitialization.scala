package org.enso.languageserver.boot.resource

import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.DirectoriesConfig

import scala.concurrent.{ExecutionContext, Future}

/** Directories initialization.
  *
  * @param directoriesConfig the directories config
  */
class DirectoriesInitialization(directoriesConfig: DirectoriesConfig)(implicit
  ec: ExecutionContext
) extends InitializationComponent
    with LazyLogging {

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future {
      directoriesConfig.createDirectories()
      logger.info("Initialized directories.")
      InitializationComponent.Initialized
    }
}
