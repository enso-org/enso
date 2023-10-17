package org.enso.languageserver.boot.resource

import java.util.concurrent.Semaphore

import scala.concurrent.{ExecutionContext, Future}

final class BlockingResourcesInitialization(component: InitializationComponent)(
  implicit ec: ExecutionContext
) extends InitializationComponent {

  private val lock = new Semaphore(1)

  /** @inheritdoc */
  override def isInitialized: Boolean =
    component.isInitialized

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] = {
    lock.acquire()
    val result = component.init()
    result.onComplete(_ => lock.release())
    result
  }
}
