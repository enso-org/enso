package org.enso.languageserver.boot.resource

import akka.event.EventStream
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.effect
import org.enso.languageserver.event.InitializedEvent

import scala.concurrent.{ExecutionContext, Future}

/** Initialization of ZIO runtime.
  *
  * @param runtime the runtime to initialize
  * @param eventStream events stream
  * @param ec the execution context
  */
class ZioRuntimeInitialization(
  runtime: effect.Runtime,
  eventStream: EventStream
)(implicit
  ec: ExecutionContext
) extends InitializationComponent
    with LazyLogging {

  @volatile
  private var _isInitialized: Boolean = false

  override def isInitialized: Boolean = _isInitialized

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    if (isInitialized) Future.successful(InitializationComponent.Initialized)
    else
      Future {
        logger.info("Initializing ZIO runtime...")
        runtime.init()
        logger.info("ZIO runtime initialized [{}].", runtime)
        _isInitialized = true
        eventStream.publish(InitializedEvent.ZioRuntimeInitialized)
        InitializationComponent.Initialized
      }
}
