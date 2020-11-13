package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.Status.Failure
import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import org.enso.languageserver.boot.{
  LanguageServerComponent,
  LanguageServerConfig
}
import org.enso.projectmanager.boot.configuration.BootloaderConfig
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerBootLoader.{
  Boot,
  FindFreeSocket,
  ServerBootFailed,
  ServerBooted
}
import org.enso.projectmanager.infrastructure.net.Tcp
import org.enso.projectmanager.util.UnhandledLogging

/** It boots a Language Sever described by the `descriptor`. Upon boot failure
  * looks up new available port and retries to boot the server.
  *
  * @param descriptor a LS descriptor
  * @param config a bootloader config
  */
class LanguageServerBootLoader(
  descriptor: LanguageServerDescriptor,
  config: BootloaderConfig
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    log.info(s"Booting a language server [$descriptor]")
    self ! FindFreeSocket
  }

  override def receive: Receive = findingSocket()

  private def findingSocket(retry: Int = 0): Receive = {
    case FindFreeSocket =>
      log.debug("Looking for available socket to bind the language server")
      val jsonRpcPort = findPort()
      var binaryPort  = findPort()
      while (binaryPort == jsonRpcPort) {
        binaryPort = findPort()
      }
      log.info(
        s"Found sockets for the language server " +
        s"[json:${descriptor.networkConfig.interface}:$jsonRpcPort, " +
        s"binary:${descriptor.networkConfig.interface}:$binaryPort]"
      )
      self ! Boot
      context.become(booting(jsonRpcPort, binaryPort, retry))

    case GracefulStop =>
      context.stop(self)
  }

  private def booting(rpcPort: Int, dataPort: Int, retryCount: Int): Receive = {
    case Boot =>
      log.debug("Booting a language server")
      val config = LanguageServerConfig(
        descriptor.networkConfig.interface,
        rpcPort,
        dataPort,
        descriptor.rootId,
        descriptor.root,
        descriptor.name,
        context.dispatcher
      )
      val server = new LanguageServerComponent(config)
      server.start().map(_ => config -> server) pipeTo self

    case Failure(th) =>
      log.error(
        th,
        s"An error occurred during boot of Language Server [${descriptor.name}]"
      )
      if (retryCount < config.numberOfRetries) {
        context.system.scheduler
          .scheduleOnce(config.delayBetweenRetry, self, FindFreeSocket)
        context.become(findingSocket(retryCount + 1))
      } else {
        log.error(
          s"Tried $retryCount times to boot Language Server. Giving up."
        )
        context.parent ! ServerBootFailed(th)
        context.stop(self)
      }

    case (config: LanguageServerConfig, server: LanguageServerComponent) =>
      log.info(s"Language server booted [$config].")
      context.parent ! ServerBooted(config, server)
      context.stop(self)

    case GracefulStop =>
      context.stop(self)
  }

  private def findPort(): Int =
    Tcp.findAvailablePort(
      descriptor.networkConfig.interface,
      descriptor.networkConfig.minPort,
      descriptor.networkConfig.maxPort
    )

}

object LanguageServerBootLoader {

  /** Creates a configuration object used to create a [[LanguageServerBootLoader]].
    *
    * @param descriptor a LS descriptor
    * @param config a bootloader config
    * @return a configuration object
    */
  def props(
    descriptor: LanguageServerDescriptor,
    config: BootloaderConfig
  ): Props =
    Props(new LanguageServerBootLoader(descriptor, config))

  /** Find free socket command.
    */
  case object FindFreeSocket

  /** Boot command.
    */
  case object Boot

  /** Signals that server boot failed.
    *
    * @param th a throwable
    */
  case class ServerBootFailed(th: Throwable)

  /** Signals that server booted successfully.
    *
    * @param config a server config
    * @param server a server lifecycle component
    */
  case class ServerBooted(
    config: LanguageServerConfig,
    server: LanguageServerComponent
  )

}
