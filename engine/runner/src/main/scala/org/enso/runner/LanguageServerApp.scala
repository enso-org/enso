package org.enso.runner

import org.enso.languageserver.boot.{
  LanguageServerComponent,
  LanguageServerConfig
}
import org.enso.loggingservice.LogLevel

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn

/** Language server runner.
  */
object LanguageServerApp {

  /** Runs a Language Server
    *
    * @param config the application config
    * @param logLevel the logging level
    * @param deamonize should the language server process be daemonized
    */
  def run(
    config: LanguageServerConfig,
    logLevel: LogLevel,
    deamonize: Boolean
  ): Unit = {
    val server = new LanguageServerComponent(config, logLevel)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      Await.result(server.stop(), 40.seconds)
    }))
    Await.result(server.start(), 1.minute)
    if (deamonize) {
      val lock = new AnyRef
      lock.synchronized {
        lock.wait()
      }
    } else {
      StdIn.readLine()
    }
  }

}
