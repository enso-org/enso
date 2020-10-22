package org.enso.runner

import org.enso.languageserver.boot.{
  LanguageServerComponent,
  LanguageServerConfig
}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn

/** Language server runner.
  */
object LanguageServerApp {

  /** Runs a Language Server
    *
    * @param config a config
    */
  def run(config: LanguageServerConfig): Unit = {
    println("Starting Language Server...")
    val server = new LanguageServerComponent(config)
    Await.result(server.start(), 10.seconds)
    StdIn.readLine()
    Await.result(server.stop(), 10.seconds)
  }

}
