package org.enso.interpreter.service

import com.typesafe.scalalogging.Logger
import org.enso.cli.task.{ProgressNotification, TaskProgress}
import org.enso.distribution.ProgressAndLockNotificationForwarder
import org.enso.editions.{LibraryName, LibraryVersion}
import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}
import org.enso.interpreter.instrument.NotificationHandler
import org.enso.interpreter.instrument.ProgressNotificationTranslator
import org.enso.interpreter.instrument.Endpoint

import java.nio.file.Path

/** A [[NotificationHandler]] for interactive mode, which forwards the
  * notifications to the Language Server, which then should forward them to
  * the IDE.
  */
private class InteractiveMode(endpoint: Endpoint)
    extends ProgressAndLockNotificationForwarder
    with NotificationHandler {
  private val logger = Logger[InteractiveMode]

  private def sendMessage(message: ApiResponse): Unit = {
    val response = Api.Response(None, message)
    endpoint.sendToClient(response)
  }

  /** @inheritdoc */
  override def addedLibrary(
    libraryName: LibraryName,
    libraryVersion: LibraryVersion,
    location: Path
  ): Unit = {
    sendMessage(
      Api.LibraryLoaded(
        namespace = libraryName.namespace,
        name      = libraryName.name,
        version   = libraryVersion.toString,
        location  = location.toFile
      )
    )
    endpoint.sendToSelf(
      Api.Request(
        Api.DeserializeLibrarySuggestions(libraryName)
      )
    )
  }

  /** @inheritdoc */
  override def serializeModule(module: QualifiedName): Unit =
    endpoint.sendToSelf(
      Api.Request(Api.SerializeModule(module))
    )

  /** @inheritdoc */
  override def trackProgress(message: String, task: TaskProgress[_]): Unit = {
    logger.info(message)
    super.trackProgress(message, task)
  }

  override def sendProgressNotification(
    notification: ProgressNotification
  ): Unit = sendMessage(
    ProgressNotificationTranslator.translate(
      "compiler/downloadingDependencies",
      notification
    )
  )
}
