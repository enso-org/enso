package org.enso.languageserver.boot

import akka.http.scaladsl.Http
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.LanguageProtocol
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
  private var maybeServerState: Option[(MainModule, Http.ServerBinding)] = None

  implicit private val ec = config.computeExecutionContext

  /** @inheritdoc **/
  override def start(): Future[ComponentStarted.type] = {
    logger.info("Starting Language Server...")
    for {
      mainModule <- Future { new MainModule(config) }
      _          <- Future { mainModule.languageServer ! LanguageProtocol.Initialize }
      binding    <- mainModule.server.bind(config.interface, config.port)
      _          <- Future { maybeServerState = Some((mainModule, binding)) }
      _ <- Future {
        logger.info(s"Started server at ${config.interface}:${config.port}")
      }
    } yield ComponentStarted
  }

  /** @inheritdoc **/
  override def stop(): Future[ComponentStopped.type] =
    maybeServerState match {
      case None =>
        Future.failed(new Exception("Server isn't running"))

      case Some((mainModule, binding)) =>
        for {
          _ <- binding.terminate(10.seconds)
          _ <- mainModule.system.terminate()
          _ <- Future { mainModule.context.close(true) }
          _ <- Future { maybeServerState = None }
        } yield ComponentStopped
    }

  /** @inheritdoc **/
  override def restart(): Future[ComponentRestarted.type] =
    for {
      _ <- forceStop()
      _ <- start()
    } yield ComponentRestarted

  private def forceStop(): Future[Unit] = {
    maybeServerState match {
      case None =>
        Future.successful(())

      case Some((mainModule, binding)) =>
        for {
          _ <- binding.terminate(10.seconds).recover(logError)
          _ <- mainModule.system.terminate().recover(logError)
          _ <- Future { mainModule.context.close(true) }.recover(logError)
          _ <- Future { maybeServerState = None }
        } yield ComponentStopped
        Future()
    }
  }

  private val logError: PartialFunction[Throwable, Unit] = {
    case th => logger.error("An error occurred during stopping server", th)
  }

}
