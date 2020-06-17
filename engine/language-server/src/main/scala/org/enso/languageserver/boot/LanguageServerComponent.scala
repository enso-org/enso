package org.enso.languageserver.boot

import akka.http.scaladsl.Http
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.boot.LanguageServerComponent.ServerContext
import org.enso.languageserver.boot.LifecycleComponent.{
  ComponentRestarted,
  ComponentStarted,
  ComponentStopped
}
import org.enso.languageserver.runtime.RuntimeKiller.{
  RuntimeShutdownResult,
  ShutDownRuntime
}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

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
      jsonBinding <- module.jsonRpcServer.bind(config.interface, config.rpcPort)
      binaryBinding <- module.binaryServer
        .bind(config.interface, config.dataPort)
      _ <- Future {
        maybeServerCtx = Some(ServerContext(module, jsonBinding, binaryBinding))
      }
      _ <- Future {
        logger.info(
          s"Started server at json:${config.interface}:${config.rpcPort}, " +
          s"binary:${config.interface}:${config.dataPort}"
        )
      }
    } yield ComponentStarted
  }

  /** @inheritdoc **/
  override def stop(): Future[ComponentStopped.type] =
    maybeServerCtx match {
      case None =>
        Future.failed(new Exception("Server isn't running"))

      case Some(serverContext) =>
        for {
          _ <- terminateTruffle(serverContext)
          _ <- terminateAkka(serverContext)
          _ <- Future { maybeServerCtx = None }
        } yield ComponentStopped
    }

  private def terminateAkka(serverContext: ServerContext): Future[Unit] = {
    for {
      _ <- serverContext.jsonBinding.terminate(2.seconds).recover(logError)
      _ <- Future { logger.info("Terminated json connections") }
      _ <- serverContext.binaryBinding.terminate(2.seconds).recover(logError)
      _ <- Future { logger.info("Terminated binary connections") }
      _ <- Await
        .ready(
          serverContext.mainModule.system.terminate().recover(logError),
          2.seconds
        )
        .recover(logError)
      _ <- Future { logger.info("Terminated actor system") }
    } yield ()
  }

  private def terminateTruffle(serverContext: ServerContext): Future[Unit] = {
    implicit val askTimeout = Timeout(12.seconds)
    val killFiber =
      (serverContext.mainModule.runtimeKiller ? ShutDownRuntime)
        .mapTo[RuntimeShutdownResult]

    for {
      _ <- killFiber.recover(logError)
      _ <- Future { logger.info("Terminated truffle context") }
    } yield ()
  }

  /** @inheritdoc **/
  override def restart(): Future[ComponentRestarted.type] =
    for {
      _ <- stop()
      _ <- start()
    } yield ComponentRestarted

  private val logError: PartialFunction[Throwable, Unit] = {
    case th => logger.error("An error occurred during stopping server", th)
  }

}

object LanguageServerComponent {

  /**
    * A running server context.
    *
    * @param mainModule a main module containing all components of the server
    * @param jsonBinding a http binding for rpc protocol
    * @param binaryBinding a http binding for data protocol
    */
  case class ServerContext(
    mainModule: MainModule,
    jsonBinding: Http.ServerBinding,
    binaryBinding: Http.ServerBinding
  )

}
