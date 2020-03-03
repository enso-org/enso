package org.enso.runner

import java.io.File

import akka.actor.{ActorSystem, Props}
import akka.stream.{ActorMaterializer, SystemMaterializer}
import cats.effect.IO
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.FileSystem
import org.enso.languageserver.{
  LanguageProtocol,
  LanguageServer,
  WebSocketServer
}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn

/**
  * Language server runner.
  */
object LanguageServerApp {

  /**
    * Runs a Language Server
    *
    * @param config a config
    */
  def run(config: LanguageServerConfig): Unit = {
    println("Starting Language Server...")
    implicit val system       = ActorSystem()
    implicit val materializer = SystemMaterializer.get(system)
    val languageServerConfig = Config(
      Map(config.contentRootUuid -> new File(config.contentRootPath))
    )
    val languageServer =
      system.actorOf(
        Props(new LanguageServer(languageServerConfig, new FileSystem[IO]))
      )

    languageServer ! LanguageProtocol.Initialize

    val server = new WebSocketServer(languageServer)

    val binding =
      Await.result(server.bind(config.interface, config.port), 3.seconds)
    println(
      s"Started server at ${config.interface}:${config.port}, press enter to kill server"
    )
    StdIn.readLine()
  }

}
