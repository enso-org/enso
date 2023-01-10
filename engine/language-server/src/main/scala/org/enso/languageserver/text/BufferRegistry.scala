package org.enso.languageserver.text

import akka.actor.{Actor, ActorRef, Cancellable, Props, Stash, Terminated}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.boot.TimingsConfig
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  CapabilityAcquisitionBadRequest,
  CapabilityReleaseBadRequest,
  ReleaseCapability
}
import org.enso.languageserver.data.{CanEdit, CapabilityRegistration, ClientId}
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.text.BufferRegistry.{
  ReloadBufferTimeout,
  SaveTimeout,
  VcsTimeout
}
import org.enso.languageserver.text.CollaborativeBuffer.{
  ForceSave,
  ReloadBuffer,
  ReloadBufferFailed,
  ReloadedBuffer
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.text.TextProtocol.{
  ApplyEdit,
  ApplyExpressionValue,
  CloseFile,
  FileNotOpened,
  FileSaved,
  OpenBuffer,
  OpenFile,
  SaveFailed,
  SaveFile
}
import org.enso.languageserver.vcsmanager.GenericVcsFailure
import org.enso.languageserver.vcsmanager.VcsProtocol.{
  InitRepo,
  RestoreRepo,
  RestoreRepoResponse,
  SaveRepo
}
import org.enso.text.ContentBasedVersioning

import java.util.UUID

/** An actor that routes request regarding text editing to the right buffer.
  * It creates a buffer actor, if a buffer doesn't exists.
  *
  * == Implementation ==
  *
  *   - 1  - Singleton
  *   - *C - Created per client.
  *   - *P - Created per file path.
  *   - *H - Request is forwarded to intermediate handler. Created per request.
  *
  * {{{
  *
  *                   *C                            1      1
  *  +------------------+   *H    +------------------+     +------------------+
  *  | ClientController +----+--->+  BufferRegistry  +---->+    VCSManager    |
  *  +------------------+    ^    +---------+--------+     +------------------+
  *                          |              |
  *                          |              |
  *                    1     |              v       *P
  *  +------------------+    |   +----------+----------+
  *  | RuntimeConnector +<---+---+ CollaborativeBuffer |
  *  +------------------+        +----------+----------+
  *                                         ^
  *                                         |
  *                                         v     1
  *                                  +------+------+
  *                                  | FileManager |
  *                                  +-------------+
  *
  * }}}
  *
  * @param fileManager a file manager
  * @param vcsManager a VCS manager
  * @param runtimeConnector a gateway to the runtime
  * @param versionCalculator a content based version calculator
  * @param timingsConfig a config with timeout/delay values
  */
class BufferRegistry(
  fileManager: ActorRef,
  vcsManager: ActorRef,
  runtimeConnector: ActorRef,
  timingsConfig: TimingsConfig
)(implicit
  versionCalculator: ContentBasedVersioning
) extends Actor
    with Stash
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    logger.info("Starting initialization.")
    context.system.eventStream
      .subscribe(self, InitializedEvent.FileVersionsRepoInitialized.getClass)
  }

  override def receive: Receive = initializing

  private def initializing: Receive = {
    case InitializedEvent.FileVersionsRepoInitialized =>
      logger.info("Initiaized.")
      context.become(running(Map.empty))
      unstashAll()

    case _ =>
      stash()
  }

  private def running(registry: Map[Path, ActorRef]): Receive = {
    case Ping =>
      sender() ! Pong

    case msg @ OpenFile(_, path) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        val bufferRef =
          context.actorOf(
            CollaborativeBuffer.props(
              path,
              fileManager,
              runtimeConnector,
              timingsConfig = timingsConfig
            ),
            s"collaborative-buffer-${UUID.randomUUID()}"
          )
        context.watch(bufferRef)
        bufferRef.forward(msg)
        context.become(running(registry + (path -> bufferRef)))
      }

    case msg @ OpenBuffer(_, path) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        val bufferRef =
          context.actorOf(
            CollaborativeBuffer.props(
              path,
              fileManager,
              runtimeConnector,
              timingsConfig = timingsConfig
            )
          )
        context.watch(bufferRef)
        bufferRef.forward(msg)
        context.become(running(registry + (path -> bufferRef)))
      }

    case Terminated(bufferRef) =>
      context.become(running(registry.filter(_._2 != bufferRef)))

    case msg @ CloseFile(_, path) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! FileNotOpened
      }

    case msg @ AcquireCapability(_, CapabilityRegistration(CanEdit(path))) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! CapabilityAcquisitionBadRequest
      }

    case msg @ ReleaseCapability(_, CapabilityRegistration(CanEdit(path))) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! CapabilityReleaseBadRequest
      }

    case msg @ ApplyEdit(_, FileEdit(path, _, _, _), _) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! FileNotOpened
      }

    case msg @ ApplyExpressionValue(_, _, path, _, _, _) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! FileNotOpened
      }

    case msg @ SaveFile(_, path, _) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! FileNotOpened
      }

    case msg @ InitRepo(clientId, path) =>
      forwardMessageToVCS((msg, sender()), clientId, path, registry)

    case msg @ SaveRepo(clientId, path, _) =>
      forwardMessageToVCS((msg, sender()), clientId, path, registry)

    case msg @ RestoreRepo(clientId, path, _) =>
      forwardMessageToVCSAndReloadBuffers(
        (msg, sender()),
        clientId,
        path,
        registry
      )
  }

  private def forwardMessageToVCS(
    msgWithSender: (Any, ActorRef),
    clientId: ClientId,
    root: Path,
    registry: Map[Path, ActorRef]
  ): Unit = {
    val openBuffers = registry.filter(_._1.startsWith(root))
    val timeouts = openBuffers.map { case (_, actorRef) =>
      actorRef ! ForceSave(clientId)
      (
        actorRef,
        context.system.scheduler
          .scheduleOnce(timingsConfig.requestTimeout, self, SaveTimeout)
      )
    }
    if (timeouts.isEmpty) {
      vcsManager.forward(msgWithSender._1)
      context.become(running(registry))
    } else {
      context.become(
        waitOnSaveConfirmationAndForwardToVCS(msgWithSender, registry, timeouts)
      )
    }
  }

  private def waitOnSaveConfirmationAndForwardToVCS(
    msg: (Any, ActorRef),
    registry: Map[Path, ActorRef],
    timeouts: Map[ActorRef, Cancellable]
  ): Receive = {
    case SaveTimeout(from) =>
      // TODO: log failure
      val timeouts1 = timeouts.removed(from)
      if (timeouts1.isEmpty) {
        vcsManager.tell(msg._1, msg._2)
        unstashAll()
        context.become(running(registry))
      } else {
        context.become(
          waitOnSaveConfirmationAndForwardToVCS(msg, registry, timeouts1)
        )
      }
    case SaveFailed | FileSaved =>
      // TODO: log failure
      timeouts.get(sender()).foreach(_.cancel())
      val timeouts1 = timeouts.removed(sender())
      if (timeouts1.isEmpty) {
        vcsManager.tell(msg._1, msg._2)
        unstashAll()
        context.become(running(registry))
      } else {
        context.become(
          waitOnSaveConfirmationAndForwardToVCS(msg, registry, timeouts1)
        )
      }
    case _ =>
      stash()
  }

  private def forwardMessageToVCSAndReloadBuffers(
    msgWithSender: (Any, ActorRef),
    clientId: ClientId,
    root: Path,
    registry: Map[Path, ActorRef]
  ): Unit = {
    val openBuffers = registry.filter(_._1.startsWith(root))
    val timeouts = openBuffers.map { case (_, actorRef) =>
      actorRef ! ForceSave(clientId)
      (
        actorRef,
        context.system.scheduler
          .scheduleOnce(timingsConfig.requestTimeout, self, SaveTimeout)
      )
    }
    if (timeouts.isEmpty) {
      vcsManager.tell(msgWithSender._1, self)
      val timeout = context.system.scheduler
        .scheduleOnce(timingsConfig.requestTimeout, self, VcsTimeout)
      context.become(
        waitOnVcsRestoreResponse(clientId, msgWithSender._2, timeout, registry)
      )
    } else {
      context.become(
        waitOnSaveConfirmationForwardToVCSAndReload(
          clientId,
          msgWithSender,
          registry,
          timeouts
        )
      )
    }
  }

  private def waitOnSaveConfirmationForwardToVCSAndReload(
    clientId: ClientId,
    msg: (Any, ActorRef),
    registry: Map[Path, ActorRef],
    timeouts: Map[ActorRef, Cancellable]
  ): Receive = {
    case SaveTimeout(from) =>
      val timeouts1 = timeouts.removed(from)
      if (timeouts1.isEmpty) {
        vcsManager ! msg._1
        val vcsTimeout = context.system.scheduler
          .scheduleOnce(timingsConfig.requestTimeout, self, SaveTimeout)
        unstashAll()
        context.become(
          waitOnVcsRestoreResponse(clientId, msg._2, vcsTimeout, registry)
        )
      } else {
        context.become(
          waitOnSaveConfirmationForwardToVCSAndReload(
            clientId,
            msg,
            registry,
            timeouts1
          )
        )
      }

    case SaveFailed | FileSaved =>
      timeouts.get(sender()).foreach(_.cancel())
      val timeouts1 = timeouts.removed(sender())
      if (timeouts1.isEmpty) {
        vcsManager ! msg._1
        val vcsTimeout = context.system.scheduler
          .scheduleOnce(timingsConfig.requestTimeout, self, VcsTimeout)
        unstashAll()
        context.become(
          waitOnVcsRestoreResponse(clientId, msg._2, vcsTimeout, registry)
        )
      } else {
        context.become(
          waitOnSaveConfirmationForwardToVCSAndReload(
            clientId,
            msg,
            registry,
            timeouts1
          )
        )
      }

    case _ =>
      stash()
  }

  private def waitOnVcsRestoreResponse(
    clientId: ClientId,
    sender: ActorRef,
    timeout: Cancellable,
    registry: Map[Path, ActorRef]
  ): Receive = {
    case response @ RestoreRepoResponse(Right(_)) =>
      if (timeout != null) timeout.cancel()
      reloadBuffers(clientId, sender, response, registry)

    case response @ RestoreRepoResponse(Left(_)) =>
      if (timeout != null) timeout.cancel()
      sender ! response
      unstashAll()
      context.become(running(registry))

    case VcsTimeout =>
      sender ! RestoreRepoResponse(Left(GenericVcsFailure("operation timeout")))
      unstashAll()
      context.become(running(registry))

    case _ =>
      stash()
  }

  private def reloadBuffers(
    clientId: ClientId,
    from: ActorRef,
    response: RestoreRepoResponse,
    registry: Map[Path, ActorRef]
  ): Unit = {
    val filesDiff = response.result.getOrElse(Nil)
    val timeouts = registry.filter(r => filesDiff.contains(r._1)).map {
      case (path, collaborativeEditor) =>
        collaborativeEditor ! ReloadBuffer(JsonSession(clientId, from), path)
        (
          path,
          context.system.scheduler
            .scheduleOnce(
              timingsConfig.requestTimeout,
              self,
              ReloadBufferTimeout(path)
            )
        )
    }

    if (timeouts.isEmpty) {
      from ! response
      context.become(running(registry))
    } else {
      context.become(
        waitingOnBuffersToReload(from, timeouts, registry, response)
      )
    }
  }

  private def waitingOnBuffersToReload(
    from: ActorRef,
    timeouts: Map[Path, Cancellable],
    registry: Map[Path, ActorRef],
    response: RestoreRepoResponse
  ): Receive = {
    case ReloadedBuffer(path) =>
      timeouts.get(path).foreach(_.cancel())
      val timeouts1 = timeouts.removed(path)
      if (timeouts1.isEmpty) {
        from ! response
        context.become(running(registry))
      } else {
        context.become(
          waitingOnBuffersToReload(from, timeouts1, registry, response)
        )
      }

    case ReloadBufferFailed(path, _) =>
      timeouts.get(path).foreach(_.cancel())
      val timeouts1 = timeouts.removed(path)
      if (timeouts1.isEmpty) {
        // TODO: log failure
        from ! response
        context.become(running(registry))
      } else {
        context.become(
          waitingOnBuffersToReload(from, timeouts1, registry, response)
        )
      }

    case ReloadBufferTimeout(path) =>
      val timeouts1 = timeouts.removed(path)
      if (timeouts1.isEmpty) {
        // TODO: log failure
        from ! response
        context.become(running(registry))
      } else {
        context.become(
          waitingOnBuffersToReload(from, timeouts1, registry, response)
        )
      }
  }

}

object BufferRegistry {

  case class SaveTimeout(ref: ActorRef)

  case class ReloadBufferTimeout(path: Path)

  case object VcsTimeout

  /** Creates a configuration object used to create a [[BufferRegistry]]
    *
    * @param fileManager a file manager actor
    * @param vcsManager a VCS manager actor
    * @param runtimeConnector a gateway to the runtime
    * @param versionCalculator a content based version calculator
    * @param timingsConfig a config with timout/delay values
    * @return a configuration object
    */
  def props(
    fileManager: ActorRef,
    vcsManager: ActorRef,
    runtimeConnector: ActorRef,
    timingsConfig: TimingsConfig
  )(implicit
    versionCalculator: ContentBasedVersioning
  ): Props =
    Props(
      new BufferRegistry(
        fileManager,
        vcsManager,
        runtimeConnector,
        timingsConfig
      )
    )

}
