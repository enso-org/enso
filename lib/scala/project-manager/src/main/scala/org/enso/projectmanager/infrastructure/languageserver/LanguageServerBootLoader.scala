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
  * Once the server is booted it can restart it on request.
  *
  * The `bootProgressTracker` has to be provided because, while the process
  * assumes that the required engine version has been pre-installed, it still
  * may sometimes need to wait on a lock (for example if an engine is being
  * uninstalled), so these events need to be reported to the progress tracker.
  *
  * @param bootProgressTracker an [[ActorRef]] that will get progress updates
  *                            related to initializing the engine
  * @param descriptor a LS descriptor
  * @param config a bootloader config
  * @param bootTimeout a finite duration that determines the deadline for
  *                    initialization of the server process, if the process
  *                    starts but fails to initialize after this timeout, it is
  *                    treated as a boot failure and the process is gracefully
  *                    stopped
  * @param executor an executor service used to start the language server
  *                 process
  */
class LanguageServerBootLoader(
  bootProgressTracker: ActorRef,
  descriptor: LanguageServerDescriptor,
  config: BootloaderConfig,
  bootTimeout: FiniteDuration,
  executor: LanguageServerExecutor
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  // TODO [RW] consider adding a stop timeout so that if the graceful stop on
  //  timed-out boot also does not work, the process can be killed forcibly
  //  (#1315)

  import context.dispatcher

  override def preStart(): Unit = {
    log.info(s"Booting a language server [$descriptor]")
    self ! FindFreeSocket
  }

  override def receive: Receive = findingSocket()

  /** First bootloader phase - looking for a free set of ports for the server.
    *
    * Once the ports are found, the process starts booting.
    */
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
      context.become(
        bootingFirstTime(
          rpcPort    = jsonRpcPort,
          dataPort   = binaryPort,
          retryCount = retry
        )
      )

    case GracefulStop =>
      context.stop(self)
  }

  /** This phase is triggered when the ports are found.
    *
    * When booting for the first time, the actor that will be notified is the
    * parent and retries are allowed (upon retry new set of ports will be tried).
    */
  private def bootingFirstTime(
    rpcPort: Int,
    dataPort: Int,
    retryCount: Int
  ): Receive = booting(
    rpcPort       = rpcPort,
    dataPort      = dataPort,
    shouldRetry   = true,
    retryCount    = retryCount,
    bootRequester = context.parent
  )

  /** A general booting phase.
    *
    * It spawns the [[LanguageServerProcess]] child actor and waits for it to
    * confirm that the server has been successfully initialized (or failed to
    * boot). Once booted it enters the running phase.
    */
  private def booting(
    rpcPort: Int,
    dataPort: Int,
    shouldRetry: Boolean,
    retryCount: Int,
    bootRequester: ActorRef
  ): Receive = {
    case Boot =>
      log.debug("Booting a language server")
      context.actorOf(
        LanguageServerProcess.props(
          progressTracker = bootProgressTracker,
          descriptor      = descriptor,
          bootTimeout     = bootTimeout,
          rpcPort         = rpcPort,
          dataPort        = dataPort,
          executor        = executor
        ),
        s"process-wrapper-${descriptor.name}"
      )

    case LanguageServerProcess.ServerTerminated(exitCode) =>
      handleBootFailure(
        shouldRetry,
        retryCount,
        bootRequester,
        s"Language server terminated with exit code $exitCode before " +
        s"finishing booting.",
        None
      )

    case LanguageServerProcess.ServerThreadFailed(throwable) =>
      handleBootFailure(
        shouldRetry,
        retryCount,
        bootRequester,
        s"Language server thread failed with $throwable.",
        Some(throwable)
      )

    case LanguageServerProcess.ServerConfirmedFinishedBooting =>
      val connectionInfo = LanguageServerConnectionInfo(
        descriptor.networkConfig.interface,
        rpcPort  = rpcPort,
        dataPort = dataPort
      )
      log.info(s"Language server booted [$connectionInfo].")

      bootRequester ! ServerBooted(connectionInfo, self)
      context.become(running(connectionInfo))

    case GracefulStop =>
      context.children.foreach(_ ! LanguageServerProcess.Stop)
  }

  /** Handles a boot failure by logging it and depending on configuration,
    * retrying or notifying the proper actor about the failure.
    */
  private def handleBootFailure(
    shouldRetry: Boolean,
    retryCount: Int,
    bootRequester: ActorRef,
    message: String,
    throwable: Option[Throwable]
  ): Unit = {
    log.warning(message)

    if (shouldRetry && retryCount < config.numberOfRetries) {
      context.system.scheduler
        .scheduleOnce(config.delayBetweenRetry, self, FindFreeSocket)
      context.become(findingSocket(retryCount + 1))
    } else {
      if (shouldRetry) {
        log.error(
          s"Tried $retryCount times to boot Language Server. Giving up."
        )
      } else {
        log.error("Failed to restart the server. Giving up.")
      }
      bootRequester ! ServerBootFailed(
        throwable.getOrElse(new RuntimeException(message))
      )
      context.stop(self)
    }
  }

  /** After successful boot, we cannot stop as it would stop our child process,
    * so we just wait for it to terminate.
    *
    * The restart command can trigger the restarting phase which consists of two
    * parts: waiting for the old process to shutdown and rebooting a new one.
    */
  private def running(connectionInfo: LanguageServerConnectionInfo): Receive = {
    case msg @ LanguageServerProcess.ServerTerminated(exitCode) =>
      log.debug(
        s"Language Server process has terminated with exit code $exitCode"
      )
      context.parent ! msg
      context.stop(self)

    case Restart =>
      context.children.foreach(_ ! LanguageServerProcess.Stop)
      context.become(
        restartingWaitingForShutdown(connectionInfo, rebootRequester = sender())
      )

    case GracefulStop =>
      context.children.foreach(_ ! LanguageServerProcess.Stop)
  }

  // TODO [RW] handling stop timeout (#1315)
  //  may also consider a stop timeout for GracefulStop and killing the process?
  /** First phase of restart waits fot the old process to shutdown and boots the
    * new process.
    */
  def restartingWaitingForShutdown(
    connectionInfo: LanguageServerConnectionInfo,
    rebootRequester: ActorRef
  ): Receive = {
    case LanguageServerProcess.ServerTerminated(exitCode) =>
      log.debug(
        s"Language Server process has terminated (as requested to reboot) " +
        s"with exit code $exitCode"
      )

      context.become(rebooting(connectionInfo, rebootRequester))
      self ! Boot

    case GracefulStop =>
      context.children.foreach(_ ! LanguageServerProcess.Stop)
  }

  /** Currently rebooting does not retry.
    *
    * We cannot directly re-use the retry logic from the initial boot, because
    * we need to keep the ports unchanged, since they are already passed to
    * other components that will try to connect there.
    */
  def rebooting(
    connectionInfo: LanguageServerConnectionInfo,
    rebootRequester: ActorRef
  ): Receive = booting(
    rpcPort       = connectionInfo.rpcPort,
    dataPort      = connectionInfo.dataPort,
    shouldRetry   = false,
    retryCount    = config.numberOfRetries,
    bootRequester = rebootRequester
  )

  private def findPort(): Int =
    Tcp.findAvailablePort(
      descriptor.networkConfig.interface,
      descriptor.networkConfig.minPort,
      descriptor.networkConfig.maxPort
    )

  private case object FindFreeSocket
  private case object Boot
}

object LanguageServerBootLoader {

  /** Creates a configuration object used to create a [[LanguageServerBootLoader]].
    *
    * @param bootProgressTracker an [[ActorRef]] that will get progress updates
    *                            related to initializing the engine
    * @param descriptor a LS descriptor
    * @param config a bootloader config
    * @param bootTimeout maximum time the server can use to boot,
    *                    does not include the time needed to install any missing
    *                    components
    * @param executor an executor service used to start the language server
    *                 process
    * @return a configuration object
    */
  def props(
    bootProgressTracker: ActorRef,
    descriptor: LanguageServerDescriptor,
    config: BootloaderConfig,
    bootTimeout: FiniteDuration,
    executor: LanguageServerExecutor
  ): Props =
    Props(
      new LanguageServerBootLoader(
        bootProgressTracker,
        descriptor,
        config,
        bootTimeout,
        executor: LanguageServerExecutor
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
