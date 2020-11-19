package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.projectmanager.boot.configuration.BootloaderConfig
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerBootLoader.{
  ServerBootFailed,
  ServerBooted
}
import org.enso.projectmanager.infrastructure.net.Tcp
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** It boots a Language Sever described by the `descriptor`. Upon boot failure
  * looks up new available port and retries to boot the server.
  *
  * @param descriptor a LS descriptor
  * @param config a bootloader config
  */
class LanguageServerBootLoader(
  bootProgressTracker: ActorRef,
  descriptor: LanguageServerDescriptor,
  config: BootloaderConfig,
  bootTimeout: FiniteDuration
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
      context.become(booting(jsonRpcPort, binaryPort, retry, context.parent))

    case GracefulStop =>
      context.stop(self)
  }

  private def booting(
    rpcPort: Int,
    dataPort: Int,
    retryCount: Int,
    replyTo: ActorRef
  ): Receive = {
    case Boot =>
      log.debug("Booting a language server")
      context.actorOf(
        LanguageServerProcess.props(
          progressTracker = bootProgressTracker,
          descriptor      = descriptor,
          bootTimeout     = bootTimeout,
          rpcPort         = rpcPort,
          dataPort        = dataPort
        ),
        s"process-wrapper-${descriptor.name}"
      )

    case LanguageServerProcess.ServerTerminated(exitCode) =>
      val message =
        s"Language server terminated with exit code $exitCode before " +
        s"finishing booting."
      log.warning(message)

      if (retryCount < config.numberOfRetries) {
        context.system.scheduler
          .scheduleOnce(config.delayBetweenRetry, self, FindFreeSocket)
        context.become(findingSocket(retryCount + 1))
      } else {
        log.error(
          s"Tried $retryCount times to boot Language Server. Giving up."
        )
        replyTo ! ServerBootFailed(new RuntimeException(message))
        context.stop(self)
      }

    case LanguageServerProcess.ServerConfirmedFinishedBooting =>
      val connectionInfo = LanguageServerConnectionInfo(
        descriptor.networkConfig.interface,
        rpcPort  = rpcPort,
        dataPort = dataPort
      )
      log.info(s"Language server booted [$connectionInfo].")
      // TODO who is the manager?
      replyTo ! ServerBooted(connectionInfo, sender())
      context.become(running)

    case GracefulStop =>
      context.children.foreach(_ ! LanguageServerProcess.Stop)
  }

  /** After successfull boot, we cannot stop as it would stop our child process,
    * so we just wait for it to terminate, acting as a proxy.
    */
  private def running: Receive = {
    case LanguageServerProcess.ServerTerminated(exitCode) =>
      log.debug(
        s"Language Server process has terminated with exit code $exitCode"
      )
      context.stop(self)

    case GracefulStop =>
      context.children.foreach(_ ! LanguageServerProcess.Stop)
  }

//  def restarting(
//    connectionInfo: LanguageServerConnectionInfo,
//    replyTo: ActorRef
//  ): Receive = {
//    case LanguageServerProcess.ServerTerminated(exitCode) =>
//      log.debug(
//        s"Language Server process has terminated (as requested to reboot) " +
//        s"with exit code $exitCode"
//      )
//
//      context.become(rebooting(connectionInfo, replyTo))
//      self ! Boot
//
//    case GracefulStop =>
//      context.children.foreach(_ ! LanguageServerProcess.Stop)
//  }
//
//  def rebooting(
//    connectionInfo: LanguageServerConnectionInfo,
//    replyTo: ActorRef
//  ): Receive = booting(
//    connectionInfo.rpcPort,
//    connectionInfo.dataPort,
//    config.numberOfRetries,
//    replyTo
//  )

  private def findPort(): Int =
    Tcp.findAvailablePort(
      descriptor.networkConfig.interface,
      descriptor.networkConfig.minPort,
      descriptor.networkConfig.maxPort
    )

  /** Find free socket command. */
  case object FindFreeSocket

  /** Boot command. */
  case object Boot

}

object LanguageServerBootLoader {

  /** Creates a configuration object used to create a [[LanguageServerBootLoader]].
    *
    * @param descriptor a LS descriptor
    * @param config a bootloader config
    * @param bootTimeout maximum time the server can use to boot,
    *                    does not include the time needed to install any missing
    *                    components
    * @return a configuration object
    */
  def props(
    bootProgressTracker: ActorRef,
    descriptor: LanguageServerDescriptor,
    config: BootloaderConfig,
    bootTimeout: FiniteDuration
  ): Props =
    Props(
      new LanguageServerBootLoader(
        bootProgressTracker,
        descriptor,
        config,
        bootTimeout
      )
    )

  /** Signals that server boot failed.
    *
    * @param th a throwable
    */
  case class ServerBootFailed(th: Throwable)

  /** Signals that server booted successfully.
    *
    * @param connectionInfo a server config
    * @param serverProcessManager an actor that manages the server process
    *                             lifecycle, currently it is
    *                             [[LanguageServerBootLoader]]
    */
  case class ServerBooted(
    connectionInfo: LanguageServerConnectionInfo,
    serverProcessManager: ActorRef
  )

  case class ServerTerminated(exitCode: Int)

}
