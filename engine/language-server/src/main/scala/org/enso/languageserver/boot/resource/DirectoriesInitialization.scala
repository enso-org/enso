package org.enso.languageserver.boot.resource

import org.enso.languageserver.data.DirectoriesConfig
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}

/** Directories initialization.
  *
  * @param directoriesConfig the directories config
  */
class DirectoriesInitialization(directoriesConfig: DirectoriesConfig)(implicit
  ec: ExecutionContext
) extends InitializationComponent {

  private val log = LoggerFactory.getLogger(this.getClass)

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future {
      directoriesConfig.createDirectories()
      log.info("Initialized directories.")
      InitializationComponent.Initialized
    }
}
