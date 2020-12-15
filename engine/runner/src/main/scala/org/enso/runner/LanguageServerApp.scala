package org.enso.runner

import org.enso.languageserver.boot.{
  LanguageServerComponent,
  LanguageServerConfig
}
import org.enso.loggingservice.LogLevel

import scala.concurrent.Await
import scala.concurrent.duration._

/** Language server runner.
  */
object LanguageServerApp {

  /** Runs a Language Server
    *
    * @param config a config
    * @param logLevel log level
    */
  def run(config: LanguageServerConfig, logLevel: LogLevel): Unit = {
    val server = new LanguageServerComponent(config, logLevel)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      Await.result(server.stop(), 20.seconds)
    }))
    Await.result(server.start(), 20.seconds)
  }

}
