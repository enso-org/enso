package org.enso.interpreter.instrument

import com.typesafe.scalalogging.Logger
import org.enso.cli.ProgressBar
import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.editions.{LibraryName, LibraryVersion}
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}

import java.nio.file.Path

/** A class that forwards notifications about loaded libraries and long-running
  * tasks to the user interface.
  */
trait NotificationHandler extends ProgressReporter {

  /** Called when a library has been loaded.
    *
    * @param libraryName name of the added library
    * @param libraryVersion selected version
    * @param location path to the location from which the library is loaded
    */
  def addedLibrary(
    libraryName: LibraryName,
    libraryVersion: LibraryVersion,
    location: Path
  ): Unit
}

object NotificationHandler {

  /** A [[NotificationHandler]] for text mode.
    *
    * It ignores library notifications and displays progress as a progress bar
    * in the CLI, as long as the stdout is connected to a terminal.
    */
  object TextMode extends NotificationHandler {

    /** @inheritdoc */
    override def addedLibrary(
      libraryName: LibraryName,
      libraryVersion: LibraryVersion,
      location: Path
    ): Unit = {
      // Library notifications are deliberately ignored in text mode.
    }

    /** @inheritdoc */
    override def trackProgress(message: String, task: TaskProgress[_]): Unit = {
      Logger[TextMode.type].info(message)
      if (System.console() != null) {
        ProgressBar.waitWithProgress(task)
      }
    }
  }

  /** A [[NotificationHandler]] that forwards messages to other
    * NotificationHandlers.
    */
  class Forwarder extends NotificationHandler {
    private var listeners: List[NotificationHandler] = Nil

    /** @inheritdoc */
    override def addedLibrary(
      libraryName: LibraryName,
      libraryVersion: LibraryVersion,
      location: Path
    ): Unit = for (listener <- listeners)
      listener.addedLibrary(libraryName, libraryVersion, location)

    /** @inheritdoc */
    override def trackProgress(message: String, task: TaskProgress[_]): Unit =
      for (listener <- listeners) listener.trackProgress(message, task)

    /** Registers a new listener. */
    def addListener(listener: NotificationHandler): Unit =
      listeners ::= listener
  }

  /** A [[NotificationHandler]] for interactive mode, which forwards the
    * notifications to the Language Server, which then should forward them to
    * the IDE.
    */
  class InteractiveMode(endpoint: Endpoint) extends NotificationHandler {
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
    ): Unit = sendMessage(
      Api.LibraryLoaded(
        namespace = libraryName.namespace,
        name      = libraryName.name,
        version   = libraryVersion.toString,
        location  = location.toFile
      )
    )

    /** @inheritdoc */
    override def trackProgress(message: String, task: TaskProgress[_]): Unit = {
      logger.info(message)
      // TODO [RW] this should be implemented once progress tracking is used by downloads
    }
  }
}
