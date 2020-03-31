package org.enso.languageserver.boot

import akka.http.scaladsl.Http
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.LanguageProtocol
import org.enso.languageserver.boot.LanguageServerComponent.{
  ServerStarted,
  ServerStopped
}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * A lifecycle component used to start and stop a Language Server.
  *
  * @param config a LS config
  */
class LanguageServerComponent(config: LanguageServerConfig)
    extends LazyLogging {

  @volatile
  private var maybeServerState: Option[(MainModule, Http.ServerBinding)] = None

  implicit private val ec = config.computeExecutionContext

  /**
    * Starts asynchronously a server.
    *
    * @return a notice that the server started successfully
    */
  def start(): Future[ServerStarted.type] = {
    logger.info("Starting Language Server...")
    for {
      mainModule <- Future { new MainModule(config) }
      _          <- Future { mainModule.languageServer ! LanguageProtocol.Initialize }
      binding    <- mainModule.server.bind(config.interface, config.port)
      _          <- Future { maybeServerState = Some(mainModule, binding) }
      _ <- Future {
        logger.info(s"Started server at ${config.interface}:${config.port}")
      }
    } yield ServerStarted
  }

  /**
    * Stops asynchronously a server.
    *
    * @return a notice that the server stopped successfully
    */
  def stop(): Future[ServerStopped.type] =
    maybeServerState match {
      case None =>
        Future.failed(new Exception("Server isn't running"))

      case Some((mainModule, binding)) =>
        for {
          _ <- binding.terminate(10.seconds)
          _ <- mainModule.system.terminate()
        } yield ServerStopped
    }

}

object LanguageServerComponent {

  case object ServerStarted

  case object ServerStopped

}
