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
import org.enso.loggingservice.LogLevel

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/** A lifecycle component used to start and stop a Language Server.
  *
  * @param config a LS config
  * @param logLevel log level for the Language Server
  */
class LanguageServerComponent(config: LanguageServerConfig, logLevel: LogLevel)
    extends LifecycleComponent
    with LazyLogging {

  @volatile
  private var maybeServerCtx: Option[ServerContext] = None

  implicit private val ec = config.computeExecutionContext

  /** @inheritdoc */
  override def start(): Future[ComponentStarted.type] = {
    logger.info("Starting Language Server...")
    val module = new MainModule(config, logLevel)
    val initMainModule =
      for {
        _ <- module.init
        _ <- Future { logger.debug("Main module initialized") }
      } yield ()
    val bindJsonServer =
      for {
        binding <- module.jsonRpcServer.bind(config.interface, config.rpcPort)
        _       <- Future { logger.debug("Json RPC server initialized") }
      } yield binding
    val bindBinaryServer =
      for {
        binding <- module.binaryServer.bind(config.interface, config.dataPort)
        _       <- Future { logger.debug("Binary server initialized") }
      } yield binding
    for {
      jsonBinding   <- bindJsonServer
      binaryBinding <- bindBinaryServer
      _ <- Future {
        maybeServerCtx = Some(ServerContext(module, jsonBinding, binaryBinding))
      }
      _ <- initMainModule
      _ <- Future {
        logger.info(
          s"Started server at json:${config.interface}:${config.rpcPort}, " +
          s"binary:${config.interface}:${config.dataPort}"
        )
      }
    } yield ComponentStarted
  }

  /** @inheritdoc */
  override def stop(): Future[ComponentStopped.type] =
    maybeServerCtx match {
      case None =>
        Future.failed(new Exception("Server isn't running"))

      case Some(serverContext) =>
        for {
          _ <- terminateTruffle(serverContext)
          _ <- terminateAkka(serverContext)
          _ <- releaseResources(serverContext)
          _ <- Future { maybeServerCtx = None }
        } yield ComponentStopped
    }

  private def releaseResources(serverContext: ServerContext): Future[Unit] =
    for {
      _ <- Future(serverContext.mainModule.close()).recover(logError)
      _ <- Future { logger.info("Terminated main module") }
    } yield ()

  private def terminateAkka(serverContext: ServerContext): Future[Unit] = {
    for {
      _ <- serverContext.jsonBinding.terminate(2.seconds).recover(logError)
      _ <- Future { logger.info("Terminated json connections") }
      _ <- serverContext.binaryBinding.terminate(2.seconds).recover(logError)
      _ <- Future { logger.info("Terminated binary connections") }
      _ <-
        Await
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

  /** @inheritdoc */
  override def restart(): Future[ComponentRestarted.type] =
    for {
      _ <- stop()
      _ <- start()
    } yield ComponentRestarted

  private val logError: PartialFunction[Throwable, Unit] = { case th =>
    logger.error("An error occurred during stopping server", th)
  }

}

object LanguageServerComponent {

  /** A running server context.
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
