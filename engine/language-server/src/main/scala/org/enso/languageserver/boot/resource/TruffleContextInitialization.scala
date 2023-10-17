package org.enso.languageserver.boot.resource

import akka.event.EventStream
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.event.InitializedEvent
import org.enso.polyglot.LanguageInfo
import org.graalvm.polyglot.Context

import scala.concurrent.{ExecutionContext, Future}

/** Initialize the Truffle context.
  *
  * @param eventStream akka events stream
  * @param truffleContext the Truffle context
  */
final class TruffleContextInitialization(
  eventStream: EventStream,
  truffleContext: Context
)(implicit
  ec: ExecutionContext
) extends InitializationComponent
    with LazyLogging {

  @volatile
  private var _isInitialized: Boolean = false

  /** @inheritdoc */
  override def isInitialized: Boolean = _isInitialized

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future {
      logger.info("Initializing Runtime context [{}]...", truffleContext)
      truffleContext.initialize(LanguageInfo.ID)
      eventStream.publish(InitializedEvent.TruffleContextInitialized)
      logger.info("Initialized Runtime context [{}].", truffleContext)
      _isInitialized = true
      InitializationComponent.Initialized
    }
}
