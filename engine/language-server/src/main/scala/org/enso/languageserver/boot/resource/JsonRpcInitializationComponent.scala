package org.enso.languageserver.boot.resource

import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.ProtocolFactory

import scala.concurrent.{ExecutionContext, Future}

/** Initialization of JSON-RPC protocol.
  *
  * @param protocolFactory the JSON-RPC protocol factory
  */
class JsonRpcInitializationComponent(protocolFactory: ProtocolFactory)(implicit
  ec: ExecutionContext
) extends InitializationComponent
    with LazyLogging {

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future {
      logger.info("Initializing JSON-RPC protocol.")
      protocolFactory.init()
      logger.info("JSON-RPC protocol initialized.")
      InitializationComponent.Initialized
    }
}
