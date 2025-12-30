package org.enso.languageserver.boot

import java.nio.file.Files
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
import org.enso.profiling.sampler.MethodsSampler
import org.slf4j.event.Level

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

/** A lifecycle component used to start and stop a Language Server.
  *
  * @param config a LS config
  * @param logLevel log level for the Language Server
  */
class LanguageServerComponent(config: LanguageServerConfig, logLevel: Level)
    extends LifecycleComponent
    with LazyLogging {

  @volatile
  private var maybeServerCtx: Option[ServerContext] = None

  implicit private val ec: ExecutionContextExecutor =
    config.computeExecutionContext

  /** @inheritdoc */
  override def start(): Future[ComponentStarted.type] = {
    logger.info("Starting Language Server...")
    val sampler = startSampling(config)
    logger.trace("Started [{}]", sampler.getClass.getName)
    val module = new MainModule(config, logLevel)
    val bindJsonServer =
      for {
        binding <- module.jsonRpcServer.bind(config.interface, config.rpcPort)
        _       <- Future { logger.trace("Server for JSON RPC is initialized") }
      } yield binding
    val bindSecureJsonServer: Future[Option[Http.ServerBinding]] = {
      config.secureRpcPort match {
        case Some(port) =>
          module.jsonRpcServer
            .bind(config.interface, port, secure = true)
            .map(Some(_))
        case None =>
          Future.successful(None)
      }
    }

    for {
      jsonBinding       <- bindJsonServer
      secureJsonBinding <- bindSecureJsonServer
      _ <- Future {
        maybeServerCtx = Some(
          ServerContext(
            sampler,
            module,
            jsonBinding,
            secureJsonBinding
          )
        )
      }
      _ <- Future {
        logger.info(
          s"Started server at json:${config.interface}:${config.rpcPort}, ${config.secureRpcPort
            .map(p => s"secure-json:${config.interface}$p")
            .getOrElse("<secure-json-not-configured>")}, " +
          s"binary:${config.interface}:${config.dataPort}${config.secureDataPort
            .map(p => s", secure-binary:${config.interface}$p")
            .getOrElse(", <secure-binary-not-configured>")}"
        )
      }
    } yield ComponentStarted
  }

  /** Start the application sampling. */
  private def startSampling(config: LanguageServerConfig): MethodsSampler = {
    val sampler = config.profilingConfig.profilingPath match {
      case Some(path) =>
        MethodsSampler.create(Files.newOutputStream(path), null)
      case None => MethodsSampler.NOOP
    }
    sampler.start()
    config.profilingConfig.profilingTime.foreach(timeout =>
      sampler.scheduleStop(timeout)
    )

    sampler
  }

  /** @inheritdoc */
  override def stop(): Future[ComponentStopped.type] =
    maybeServerCtx match {
      case None =>
        Future.successful(ComponentStopped)

      case Some(serverContext) =>
        for {
          _ <- stopSampling(serverContext)
          _ <- terminateTruffle(serverContext)
          _ <- terminateAkka(serverContext)
          _ <- releaseResources(serverContext)
          _ <- Future { maybeServerCtx = None }
        } yield ComponentStopped
    }

  private def stopSampling(serverContext: ServerContext): Future[Unit] =
    Future(serverContext.sampler.close()).recover(logError)

  private def releaseResources(serverContext: ServerContext): Future[Unit] =
    for {
      _ <- Future(serverContext.mainModule.close()).recover(logError)
      _ <- Future { logger.debug("Terminated main module") }
    } yield ()

  private def terminateAkka(serverContext: ServerContext): Future[Unit] = {
    for {
      _ <- serverContext.jsonBinding.terminate(2.seconds).recover[Any](logError)
      _ <- Future { logger.info("Terminated JSON connections") }
      _ <-
        Await
          .ready(
            serverContext.mainModule.system.terminate().recover[Any](logError),
            2.seconds
          )
          .recover(logError)
      _ <- Future { logger.trace("Terminated actor system") }
    } yield ()
  }

  private def terminateTruffle(serverContext: ServerContext): Future[Unit] = {
    implicit val askTimeout: Timeout = Timeout(12.seconds)
    val killFiber =
      (serverContext.mainModule.runtimeKiller ? ShutDownRuntime)
        .mapTo[RuntimeShutdownResult]

    for {
      _ <- killFiber.recover[Any](logError)
      _ <- Future { logger.debug("Terminated Truffle context") }
    } yield ()
  }

  /** @inheritdoc */
  override def restart(): Future[ComponentRestarted.type] =
    for {
      _ <- stop()
      _ <- start()
    } yield ComponentRestarted

  private val logError: PartialFunction[Throwable, Unit] = { case th =>
    logger.error("An error occurred when shutting down the server", th)
  }

}

object LanguageServerComponent {

  /** A running server context.
    *
    * @param sampler a sampler gathering the application performance statistics
    * @param mainModule a main module containing all components of the server
    * @param jsonBinding an http binding for rpc protocol
    * @param secureJsonBinding an optional https binding for rpc protocol
    */
  case class ServerContext(
    sampler: MethodsSampler,
    mainModule: MainModule,
    jsonBinding: Http.ServerBinding,
    secureJsonBinding: Option[Http.ServerBinding]
  )

}
