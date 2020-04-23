package org.enso.languageserver.boot

import akka.http.scaladsl.Http
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.LanguageProtocol
import org.enso.languageserver.boot.LanguageServerComponent.ServerContext
import org.enso.languageserver.boot.LifecycleComponent.{
  ComponentRestarted,
  ComponentStarted,
  ComponentStopped
}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * A lifecycle component used to start and stop a Language Server.
  *
  * @param config a LS config
  */
class LanguageServerComponent(config: LanguageServerConfig)
    extends LifecycleComponent
    with LazyLogging {

  @volatile
  private var maybeServerCtx: Option[ServerContext] = None

  implicit private val ec = config.computeExecutionContext

  /** @inheritdoc **/
  override def start(): Future[ComponentStarted.type] = {
    logger.info("Starting Language Server...")
    for {
      module      <- Future { new MainModule(config) }
      _           <- Future { module.languageServer ! LanguageProtocol.Initialize }
      rpcBinding  <- module.jsonRpcServer.bind(config.interface, config.rpcPort)
      dataBinding <- module.dataServer.bind(config.interface, config.dataPort)
      _ <- Future {
        maybeServerCtx = Some(ServerContext(module, rpcBinding, dataBinding))
      }
      _ <- Future {
        logger.info(
          s"Started server at rpc:${config.interface}:${config.rpcPort}, " +
          s"data:${config.interface}:${config.dataPort}"
        )
      }
    } yield ComponentStarted
  }

  /** @inheritdoc **/
  override def stop(): Future[ComponentStopped.type] =
    maybeServerCtx match {
      case None =>
        Future.failed(new Exception("Server isn't running"))

      case Some(serverState) =>
        for {
          _ <- serverState.rpcBinding.terminate(10.seconds)
          _ <- serverState.dataBinding.terminate(10.seconds)
          _ <- serverState.mainModule.system.terminate()
          _ <- Future { serverState.mainModule.context.close(true) }
          _ <- Future { maybeServerCtx = None }
        } yield ComponentStopped
    }

  /** @inheritdoc **/
  override def restart(): Future[ComponentRestarted.type] =
    for {
      _ <- forceStop()
      _ <- start()
    } yield ComponentRestarted

  private def forceStop(): Future[Unit] = {
    maybeServerCtx match {
      case None =>
        Future.successful(())

      case Some(serverState) =>
        for {
          _ <- serverState.rpcBinding.terminate(10.seconds).recover(logError)
          _ <- serverState.dataBinding.terminate(10.seconds).recover(logError)
          _ <- serverState.mainModule.system.terminate().recover(logError)
          _ <- Future { serverState.mainModule.context.close(true) }
            .recover(logError)
          _ <- Future { maybeServerCtx = None }
        } yield ()
    }
  }

  private val logError: PartialFunction[Throwable, Unit] = {
    case th => logger.error("An error occurred during stopping server", th)
  }

}

object LanguageServerComponent {

  /**
    * A running server context.
    *
    * @param mainModule a main module containing all components of the server
    * @param rpcBinding a http binding for rpc protocol
    * @param dataBinding a http binding for data protocol
    */
  case class ServerContext(
    mainModule: MainModule,
    rpcBinding: Http.ServerBinding,
    dataBinding: Http.ServerBinding
  )

}
