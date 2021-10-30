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
class TruffleContextInitialization(
  eventStream: EventStream,
  truffleContext: Context
)(implicit
  ec: ExecutionContext
) extends InitializationComponent
    with LazyLogging {

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future {
      truffleContext.initialize(LanguageInfo.ID)
      eventStream.publish(InitializedEvent.TruffleContextInitialized)
      logger.info("Initialized Runtime context.")
      InitializationComponent.Initialized
    }
}
