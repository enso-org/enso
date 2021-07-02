package org.enso.interpreter.runtime

import com.typesafe.scalalogging.Logger
import org.enso.cli.ProgressBar
import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.editions.{LibraryName, LibraryVersion}
import org.enso.interpreter.instrument.Endpoint
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}

import java.nio.file.Path

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

  /** A [[NotificationHandler]] for interactive mode, which forwards the
    * notifications to the Language Server, which then should forward them to
    * the IDE.
    */
  class InteractiveMode extends NotificationHandler {
    private var endpoint: Option[Endpoint] = None
    private var queue: List[Api.Response]  = Nil

    /** Sends a message or queues it if the endpoint is not yet registered. */
    private def sendMessage(message: ApiResponse): Unit = {
      val response = Api.Response(None, message)
      endpoint match {
        case Some(connected) => connected.sendToClient(response)
        case None =>
          this.synchronized {
            endpoint match {
              case Some(justConnected) => justConnected.sendToClient(response)
              case None =>
                queue ::= response
            }
          }
      }
    }

    /** @inheritdoc */
    override def addedLibrary(
      libraryName: LibraryName,
      libraryVersion: LibraryVersion,
      location: Path
    ): Unit = sendMessage(
      Api.LibraryLoaded(libraryName, libraryVersion, location.toFile)
    )

    /** Registers the endpoint and sends to it any pending messages. */
    def registerLanguageServerEndpoint(endpoint: Endpoint): Unit =
      this.synchronized {
        if (this.endpoint.isDefined) {
          Logger[InteractiveMode].warn(
            "Language Server endpoint has been set twice. " +
            "The second one has been ignored."
          )
        } else {
          this.endpoint = Some(endpoint)
          queue.foreach(endpoint.sendToClient)
          queue = Nil
        }
      }

    /** @inheritdoc */
    override def trackProgress(message: String, task: TaskProgress[_]): Unit = {
      // TODO [RW] this should be implemented once progress tracking is used by downloads
    }
  }
}
