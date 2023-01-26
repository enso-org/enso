package org.enso.languageserver.text

import akka.actor.{Actor, ActorRef, Cancellable, Props, Stash}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.boot.TimingsConfig
import org.enso.languageserver.capability.CapabilityProtocol._
import org.enso.languageserver.data.{CanEdit, CapabilityRegistration, ClientId}
import org.enso.languageserver.event.{
  BufferClosed,
  BufferOpened,
  JsonSessionTerminated
}
import org.enso.languageserver.filemanager.FileManagerProtocol.{
  ReadTextualFileResult,
  TextualFileContent,
  WriteFileResult
}
import org.enso.languageserver.filemanager.{
  FileEventKind,
  FileManagerProtocol,
  FileNotFound,
  OperationTimeout,
  Path
}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.text.CollaborativeBuffer.{
  AutoSave,
  ForceSave,
  IOTimeout,
  ReloadBuffer,
  ReloadBufferFailed,
  ReloadedBuffer
}
import org.enso.languageserver.text.TextProtocol._
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ExpressionId
import org.enso.text.{ContentBasedVersioning, ContentVersion}
import org.enso.text.editing._
import org.enso.text.editing.model.TextEdit

/** An actor enabling multiple users edit collaboratively a file.
  *
  * @param bufferPath a path to a file
  * @param fileManager a file manger actor
  * @param runtimeConnector a gateway to the runtime
  * @param versionCalculator a content based version calculator
  * @param timingsConfig a config with timeout/delay values
  */
class CollaborativeBuffer(
  bufferPath: Path,
  fileManager: ActorRef,
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
    context.system.eventStream
      .subscribe(self, classOf[JsonSessionTerminated]): Unit
  }

  override def receive: Receive = uninitialized

  private def uninitialized: Receive = {
    case OpenFile(client, path) =>
      context.system.eventStream.publish(BufferOpened(path))
      logger.info(
        "Buffer opened for [path:{}, client:{}].",
        path,
        client.clientId
      )
      readFile(client, path)

    case OpenBuffer(client, path) =>
      context.system.eventStream.publish(BufferOpened(path))
      logger.info(
        "Buffer opened in-memory for [path:{}, client:{}].",
        path,
        client.clientId
      )
      openBuffer(client, path)
  }

  private def waitingForFileContent(
    rpcSession: JsonSession,
    replyTo: ActorRef,
    timeoutCancellable: Cancellable,
    inMemoryBuffer: Boolean
  ): Receive = {
    case ReadTextualFileResult(Right(content)) =>
      handleFileContent(rpcSession, replyTo, content, inMemoryBuffer, Map.empty)
      unstashAll()
      timeoutCancellable.cancel()

    case ReadTextualFileResult(Left(failure)) =>
      replyTo ! OpenFileResponse(Left(failure))
      timeoutCancellable.cancel()
      stop(Map.empty)

    case IOTimeout =>
      replyTo ! OpenFileResponse(Left(OperationTimeout))
      stop(Map.empty)

    case _ => stash()
  }

  private def collaborativeEditing(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Receive = {
    case OpenFile(client, _) =>
      openFile(buffer, clients, lockHolder, client, autoSave)

    case AcquireCapability(client, CapabilityRegistration(CanEdit(path))) =>
      acquireWriteLock(buffer, clients, lockHolder, client, path, autoSave)

    case ReleaseCapability(client, CapabilityRegistration(CanEdit(_))) =>
      releaseWriteLock(buffer, clients, lockHolder, client.clientId, autoSave)

    case JsonSessionTerminated(client) =>
      if (clients.contains(client.clientId)) {
        removeClient(buffer, clients, lockHolder, client.clientId, autoSave)
      }

    case CloseFile(clientId, _) =>
      if (clients.contains(clientId)) {
        autoSave.get(clientId) match {
          case Some((contentVersion, cancellable)) =>
            cancellable.cancel()
            saveFile(
              buffer,
              clients,
              lockHolder,
              clientId,
              contentVersion,
              autoSave,
              isAutoSave     = true,
              onClose        = Some(clientId),
              reportProgress = Some(sender())
            )
          case None =>
            removeClient(buffer, clients, lockHolder, clientId, autoSave)
            sender() ! FileClosed
        }
      } else {
        sender() ! FileNotOpened
      }

    case ApplyEdit(clientId, change, execute) =>
      edit(buffer, clients, lockHolder, clientId, change, execute, autoSave)

    case ApplyExpressionValue(
          clientId,
          expressionId,
          path,
          edit,
          oldVersion,
          newVersion
        ) =>
      val change = FileEdit(path, List(edit), oldVersion, newVersion)
      editExpressionValue(
        buffer,
        clients,
        lockHolder,
        clientId,
        change,
        expressionId,
        edit.text,
        autoSave
      )

    case SaveFile(clientId, _, clientVersion) =>
      saveFile(
        buffer,
        clients,
        lockHolder,
        clientId,
        ContentVersion(clientVersion),
        autoSave,
        isAutoSave = false,
        onClose    = None
      )

    case AutoSave(clientId, clientVersion) =>
      saveFile(
        buffer,
        clients,
        lockHolder,
        clientId,
        clientVersion,
        autoSave.removed(clientId),
        isAutoSave = true,
        onClose    = None
      )

    case ForceSave(clientId) =>
      autoSave.get(clientId) match {
        case Some((contentVersion, cancellable)) =>
          cancellable.cancel()
          saveFile(
            buffer,
            clients,
            lockHolder,
            clientId,
            contentVersion,
            autoSave.removed(clientId),
            isAutoSave     = true,
            onClose        = None,
            reportProgress = Some(sender())
          )
        case None =>
          sender() ! FileSaved
      }

    case ReloadBuffer(rpcSession, path) =>
      if (buffer.inMemory) {
        fileManager ! FileManagerProtocol.OpenBuffer(path)
      } else {
        fileManager ! FileManagerProtocol.ReadFile(path)
      }
      val timeoutCancellable = context.system.scheduler
        .scheduleOnce(timingsConfig.requestTimeout, self, IOTimeout)
      context.become(
        waitingOnReloadedContent(
          sender(),
          rpcSession,
          path,
          buffer,
          timeoutCancellable,
          clients,
          buffer.inMemory
        )
      )

  }

  private def waitingOnReloadedContent(
    replyTo: ActorRef,
    rpcSession: JsonSession,
    path: Path,
    oldBuffer: Buffer,
    timeoutCancellable: Cancellable,
    clients: Map[ClientId, JsonSession],
    inMemoryBuffer: Boolean
  ): Receive = {
    case ReadTextualFileResult(Right(file)) =>
      timeoutCancellable.cancel()
      val buffer = Buffer(file.path, file.content, inMemoryBuffer)

      // Notify *all* clients about the new buffer
      // This also ensures that the client that requested the restore operation
      // also gets a notification.
      val change = FileEdit(
        path,
        List(TextEdit(oldBuffer.fullRange, file.content)),
        oldBuffer.version.toHexString,
        buffer.version.toHexString
      )
      runtimeConnector ! Api.Request(
        Api.SetModuleSourcesNotification(file.path, file.content)
      )
      clients.values.foreach { _.rpcController ! TextDidChange(List(change)) }
      unstashAll()
      replyTo ! ReloadedBuffer(path)
      context.become(
        collaborativeEditing(
          buffer,
          clients,
          lockHolder = Some(rpcSession),
          Map.empty
        )
      )

    case ReadTextualFileResult(Left(FileNotFound)) =>
      clients.values.foreach {
        _.rpcController ! TextProtocol.FileEvent(path, FileEventKind.Removed)
      }
      replyTo ! ReloadedBuffer(path)
      timeoutCancellable.cancel()
      stop(Map.empty)

    case ReadTextualFileResult(Left(err)) =>
      replyTo ! ReloadBufferFailed(path, "io failure: " + err.toString)
      timeoutCancellable.cancel()
      context.become(
        collaborativeEditing(
          oldBuffer,
          clients,
          lockHolder = Some(rpcSession),
          Map.empty
        )
      )

    case IOTimeout =>
      replyTo ! ReloadBufferFailed(path, "io timeout")
      context.become(
        collaborativeEditing(
          oldBuffer,
          clients,
          lockHolder = Some(rpcSession),
          Map.empty
        )
      )

    case _ => stash()
  }

  private def saving(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    autoSave: Map[ClientId, (ContentVersion, Cancellable)],
    lockHolder: Option[JsonSession],
    replyTo: Option[ActorRef],
    onClose: Option[ClientId],
    timeoutCancellable: Cancellable
  ): Receive = {
    case IOTimeout =>
      replyTo.foreach(_ ! SaveFailed(OperationTimeout))
      unstashAll()
      onClose match {
        case Some(clientId) =>
          replyTo.foreach(_ ! FileClosed)
          removeClient(buffer, clients, lockHolder, clientId, autoSave)
        case None =>
          context.become(
            collaborativeEditing(buffer, clients, lockHolder, autoSave)
          )
      }

    case WriteFileResult(Left(failure)) =>
      replyTo.foreach(_ ! SaveFailed(failure))
      unstashAll()
      timeoutCancellable.cancel()
      onClose match {
        case Some(clientId) =>
          replyTo.foreach(_ ! FileClosed)
          removeClient(buffer, clients, lockHolder, clientId, autoSave)
        case None =>
          context.become(
            collaborativeEditing(buffer, clients, lockHolder, autoSave)
          )
      }

    case WriteFileResult(Right(())) =>
      replyTo match {
        case Some(replyTo) => replyTo ! FileSaved
        case None =>
          clients.values.foreach {
            _.rpcController ! FileAutoSaved(bufferPath)
          }
      }
      unstashAll()
      timeoutCancellable.cancel()
      onClose match {
        case Some(clientId) =>
          replyTo.foreach(_ ! FileClosed)
          removeClient(buffer, clients, lockHolder, clientId, autoSave)
        case None =>
          context.become(
            collaborativeEditing(buffer, clients, lockHolder, autoSave)
          )
      }

    case _ =>
      stash()
  }

  private def saveFile(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    clientId: ClientId,
    clientVersion: ContentVersion,
    currentAutoSaves: Map[ClientId, (ContentVersion, Cancellable)],
    isAutoSave: Boolean,
    onClose: Option[ClientId],
    reportProgress: Option[ActorRef] = None
  ): Unit = {
    val hasLock = lockHolder.exists(_.clientId == clientId)
    if (hasLock) {
      if (clientVersion == buffer.version) {
        fileManager ! FileManagerProtocol.WriteFile(
          bufferPath,
          buffer.contents.toString
        )
        currentAutoSaves.get(clientId).foreach(_._2.cancel())

        val timeoutCancellable = context.system.scheduler
          .scheduleOnce(timingsConfig.requestTimeout, self, IOTimeout)
        context.become(
          saving(
            buffer,
            clients,
            currentAutoSaves.removed(clientId),
            lockHolder,
            if (isAutoSave) reportProgress else Some(sender()),
            onClose,
            timeoutCancellable
          )
        )
      } else if (!isAutoSave) {
        sender() ! SaveFileInvalidVersion(
          clientVersion.toHexString,
          buffer.version.toHexString
        )
      } else {
        reportProgress.foreach(_ ! SaveFailed)
      }
    } else {
      if (!isAutoSave) {
        sender() ! SaveDenied
      }
      reportProgress.foreach(_ ! SaveFailed)
    }
  }

  private def upsertAutoSaveTimer(
    currentAutoSave: Map[ClientId, (ContentVersion, Cancellable)],
    clientId: ClientId,
    clientVersion: ContentVersion
  ): Map[ClientId, (ContentVersion, Cancellable)] = {
    currentAutoSave.get(clientId).foreach(_._2.cancel())
    val updatedAutoSave = currentAutoSave.removed(clientId)
    timingsConfig.autoSaveDelay
      .map(delay =>
        updatedAutoSave.updated(
          clientId,
          (
            clientVersion,
            context.system.scheduler
              .scheduleOnce(
                delay,
                self,
                AutoSave(clientId, clientVersion)
              )
          )
        )
      )
      .getOrElse(updatedAutoSave)
  }

  private def editExpressionValue(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    clientId: ClientId,
    change: FileEdit,
    expressionId: ExpressionId,
    expressionValue: String,
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Unit = {
    applyEdits(buffer, lockHolder, clientId, change) match {
      case Left(failure) =>
        sender() ! failure

      case Right(modifiedBuffer) =>
        sender() ! ApplyEditSuccess
        val subscribers = clients.filterNot(_._1 == clientId).values
        subscribers foreach { _.rpcController ! TextDidChange(List(change)) }
        runtimeConnector ! Api.Request(
          Api.SetExpressionValueNotification(
            buffer.file,
            change.edits,
            expressionId,
            expressionValue
          )
        )
        val newAutoSave: Map[ClientId, (ContentVersion, Cancellable)] =
          if (buffer.inMemory) autoSave
          else upsertAutoSaveTimer(autoSave, clientId, modifiedBuffer.version)
        context.become(
          collaborativeEditing(modifiedBuffer, clients, lockHolder, newAutoSave)
        )
    }
  }

  private def edit(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    clientId: ClientId,
    change: FileEdit,
    execute: Boolean,
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Unit = {
    applyEdits(buffer, lockHolder, clientId, change) match {
      case Left(failure) =>
        sender() ! failure

      case Right(modifiedBuffer) =>
        sender() ! ApplyEditSuccess
        val subscribers = clients.filterNot(_._1 == clientId).values
        subscribers foreach { _.rpcController ! TextDidChange(List(change)) }
        runtimeConnector ! Api.Request(
          Api.EditFileNotification(buffer.file, change.edits, execute)
        )
        val newAutoSave: Map[ClientId, (ContentVersion, Cancellable)] =
          upsertAutoSaveTimer(
            autoSave,
            clientId,
            modifiedBuffer.version
          )
        context.become(
          collaborativeEditing(modifiedBuffer, clients, lockHolder, newAutoSave)
        )
    }
  }

  private def applyEdits(
    buffer: Buffer,
    lockHolder: Option[JsonSession],
    clientId: ClientId,
    change: FileEdit
  ): Either[ApplyEditFailure, Buffer] = {
    for {
      _              <- validateAccess(lockHolder, clientId)
      _              <- validateVersions(ContentVersion(change.oldVersion), buffer.version)
      modifiedBuffer <- doEdit(buffer, change.edits)
      _ <- validateVersions(
        ContentVersion(change.newVersion),
        modifiedBuffer.version
      )
    } yield modifiedBuffer
  }

  private def validateVersions(
    clientVersion: ContentVersion,
    serverVersion: ContentVersion
  ): Either[ApplyEditFailure, Unit] = {
    if (clientVersion == serverVersion) {
      Right(())
    } else {
      Left(
        TextEditInvalidVersion(
          clientVersion.toHexString,
          serverVersion.toHexString
        )
      )
    }
  }

  private def validateAccess(
    lockHolder: Option[JsonSession],
    clientId: ClientId
  ): Either[ApplyEditFailure, Unit] = {
    val hasLock = lockHolder.exists(_.clientId == clientId)
    if (hasLock) {
      Right(())
    } else {
      Left(WriteDenied)
    }
  }

  private def doEdit(
    buffer: Buffer,
    edits: List[TextEdit]
  ): Either[ApplyEditFailure, Buffer] = {
    EditorOps
      .applyEdits(buffer.contents, edits)
      .leftMap(toEditFailure)
      .map(rope =>
        Buffer(
          buffer.file,
          rope,
          buffer.inMemory,
          versionCalculator.evalVersion(rope.toString)
        )
      )
  }

  private val toEditFailure: TextEditValidationFailure => ApplyEditFailure = {
    case EndPositionBeforeStartPosition =>
      TextEditValidationFailed("The start position is after the end position")
    case NegativeCoordinateInPosition =>
      TextEditValidationFailed("Negative coordinate in a position object")
    case InvalidPosition(position) =>
      TextEditValidationFailed(s"Invalid position: $position")
  }

  private def readFile(
    rpcSession: JsonSession,
    path: Path
  ): Unit = {
    fileManager ! FileManagerProtocol.ReadFile(path)
    val timeoutCancellable = context.system.scheduler
      .scheduleOnce(timingsConfig.requestTimeout, self, IOTimeout)
    context.become(
      waitingForFileContent(
        rpcSession,
        sender(),
        timeoutCancellable,
        inMemoryBuffer = false
      )
    )
  }

  private def openBuffer(
    rpcSession: JsonSession,
    path: Path
  ): Unit = {
    fileManager ! FileManagerProtocol.OpenBuffer(path)
    val timeoutCancellable = context.system.scheduler
      .scheduleOnce(timingsConfig.requestTimeout, self, IOTimeout)
    context.become(
      waitingForFileContent(
        rpcSession,
        sender(),
        timeoutCancellable,
        inMemoryBuffer = true
      )
    )
  }

  private def handleFileContent(
    rpcSession: JsonSession,
    originalSender: ActorRef,
    file: TextualFileContent,
    inMemoryBuffer: Boolean,
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Unit = {
    val buffer = Buffer(file.path, file.content, inMemoryBuffer)
    val cap    = CapabilityRegistration(CanEdit(bufferPath))
    originalSender ! OpenFileResponse(
      Right(OpenFileResult(buffer, Some(cap)))
    )
    runtimeConnector ! Api.Request(
      Api.SetModuleSourcesNotification(file.path, file.content)
    )
    context.become(
      collaborativeEditing(
        buffer,
        Map(rpcSession.clientId -> rpcSession),
        Some(rpcSession),
        autoSave
      )
    )
  }

  private def openFile(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    rpcSession: JsonSession,
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Unit = {
    val writeCapability =
      if (lockHolder.isEmpty)
        Some(CapabilityRegistration(CanEdit(bufferPath)))
      else
        None
    sender() ! OpenFileResponse(Right(OpenFileResult(buffer, writeCapability)))
    context.become(
      collaborativeEditing(
        buffer,
        clients + (rpcSession.clientId -> rpcSession),
        lockHolder,
        autoSave
      )
    )
  }

  private def removeClient(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    clientId: ClientId,
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Unit = {
    val newLock =
      lockHolder.flatMap {
        case holder if holder.clientId == clientId => None
        case holder                                => Some(holder)
      }

    val newClientMap = clients - clientId
    if (newClientMap.isEmpty) {
      runtimeConnector ! Api.Request(Api.CloseFileNotification(buffer.file))
      stop(autoSave)
    } else {
      context.become(
        collaborativeEditing(
          buffer,
          newClientMap,
          newLock,
          autoSave.removed(clientId)
        )
      )
    }
  }

  private def releaseWriteLock(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    clientId: ClientId,
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Unit = {
    lockHolder match {
      case None =>
        sender() ! CapabilityReleaseBadRequest
        context.become(
          collaborativeEditing(buffer, clients, lockHolder, autoSave)
        )

      case Some(holder) if holder.clientId != clientId =>
        sender() ! CapabilityReleaseBadRequest
        context.become(
          collaborativeEditing(buffer, clients, lockHolder, autoSave)
        )

      case Some(_) =>
        sender() ! CapabilityReleased
        context.become(collaborativeEditing(buffer, clients, None, autoSave))
    }
  }

  private def acquireWriteLock(
    buffer: Buffer,
    clients: Map[ClientId, JsonSession],
    lockHolder: Option[JsonSession],
    clientId: JsonSession,
    path: Path,
    autoSave: Map[ClientId, (ContentVersion, Cancellable)]
  ): Unit = {
    lockHolder match {
      case None =>
        sender() ! CapabilityAcquired
        context.become(
          collaborativeEditing(buffer, clients, Some(clientId), autoSave)
        )

      case Some(holder) if holder == clientId =>
        sender() ! CapabilityAcquisitionBadRequest
        context.become(
          collaborativeEditing(buffer, clients, lockHolder, autoSave)
        )

      case Some(holder) =>
        sender() ! CapabilityAcquired
        holder.rpcController ! CapabilityForceReleased(
          CapabilityRegistration(CanEdit(path))
        )
        context.become(
          collaborativeEditing(buffer, clients, Some(clientId), autoSave)
        )
    }
  }

  def stop(autoSave: Map[ClientId, (ContentVersion, Cancellable)]): Unit = {
    autoSave.foreach { case (_, (_, cancellable)) => cancellable.cancel() }
    context.system.eventStream.publish(BufferClosed(bufferPath))
    context.stop(self)
  }

}

object CollaborativeBuffer {

  case object IOTimeout

  private case class AutoSave(
    clientId: ClientId,
    clientVersion: ContentVersion
  )

  case class ForceSave(clientId: ClientId)

  case class ReloadBuffer(rpcSession: JsonSession, path: Path)

  case class ReloadBufferFailed(path: Path, reason: String)

  case class ReloadedBuffer(path: Path)

  /** Creates a configuration object used to create a [[CollaborativeBuffer]]
    *
    * @param bufferPath a path to a file
    * @param fileManager a file manager actor
    * @param runtimeConnector a gateway to the runtime
    * @param timingsConfig a config with timing/delay values
    * @param versionCalculator a content based version calculator
    * @return a configuration object
    */
  def props(
    bufferPath: Path,
    fileManager: ActorRef,
    runtimeConnector: ActorRef,
    timingsConfig: TimingsConfig
  )(implicit versionCalculator: ContentBasedVersioning): Props =
    Props(
      new CollaborativeBuffer(
        bufferPath,
        fileManager,
        runtimeConnector,
        timingsConfig
      )
    )

}
