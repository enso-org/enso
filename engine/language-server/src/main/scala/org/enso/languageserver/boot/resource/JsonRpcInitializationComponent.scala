package org.enso.languageserver.boot.resource

import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.ProtocolFactory

import scala.concurrent.{ExecutionContext, Future}

/** Initialization of JSON-RPC protocol.
  *
  * @param protocolFactory the JSON-RPC protocol factory
  */
final class JsonRpcInitializationComponent(protocolFactory: ProtocolFactory)(
  implicit ec: ExecutionContext
) extends InitializationComponent
    with LazyLogging {

  @volatile
  private var _isInitialized: Boolean = false

  /** @inheritdoc */
  override def isInitialized: Boolean = _isInitialized

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future {
      logger.info("Initializing JSON-RPC protocol.")
      protocolFactory.init()
      logger.info("JSON-RPC protocol initialized.")
      _isInitialized = true
      InitializationComponent.Initialized
    }

}
