package org.enso.runner

import org.enso.languageserver.boot.{LanguageServerConfig, MainModule}
import org.enso.languageserver.LanguageProtocol

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
    val mainModule = new MainModule(config)

    mainModule.languageServer ! LanguageProtocol.Initialize

    val binding =
      Await.result(
        mainModule.server.bind(config.interface, config.port),
        3.seconds
      )

    println(
      s"Started server at ${config.interface}:${config.port}, press enter to kill server"
    )
    StdIn.readLine()
    binding.terminate(10.seconds)
  }

}
