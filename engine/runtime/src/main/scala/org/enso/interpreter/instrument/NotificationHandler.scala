package org.enso.interpreter.instrument

import com.typesafe.scalalogging.Logger
import org.enso.cli.ProgressBar
import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.distribution.locking.{LockUserInterface, Resource}
import org.enso.editions.{LibraryName, LibraryVersion}
import org.enso.pkg.QualifiedName

import java.nio.file.Path

/** A class that forwards notifications about loaded libraries, locks and
  * long-running tasks to the user interface.
  */
trait NotificationHandler extends ProgressReporter with LockUserInterface {

  /** Called when a library has been loaded.
    *
    * @param libraryName    name of the added library
    * @param libraryVersion selected version
    * @param location       path to the location from which the library is loaded
    */
  def addedLibrary(
    libraryName: LibraryName,
    libraryVersion: LibraryVersion,
    location: Path
  ): Unit

  /** A request to serialize the module.
    *
    * @param moduleName qualified module name
    */
  def serializeModule(moduleName: QualifiedName): Unit
}

object NotificationHandler {

  /** A [[NotificationHandler]] for text mode.
    *
    * It ignores library notifications and displays progress as a progress bar
    * in the CLI, as long as the stdout is connected to a terminal.
    */
  object TextMode extends NotificationHandler {

    private lazy val logger = Logger[TextMode.type]

    /** @inheritdoc */
    override def addedLibrary(
      libraryName: LibraryName,
      libraryVersion: LibraryVersion,
      location: Path
    ): Unit = {
      // Library notifications are deliberately ignored in text mode.
    }

    /** @inheritdoc */
    override def serializeModule(module: QualifiedName): Unit = ()

    /** @inheritdoc */
    override def trackProgress(message: String, task: TaskProgress[_]): Unit = {
      logger.info(message)
      // TODO [RW] check the hideProgress flag provided by the launcher
      if (System.console() != null) {
        ProgressBar.waitWithProgress(task)
      }
    }

    /** @inheritdoc */
    override def startWaitingForResource(resource: Resource): Unit =
      logger.warn(resource.waitMessage)

    /** @inheritdoc */
    override def finishWaitingForResource(resource: Resource): Unit = ()
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
    def serializeModule(module: QualifiedName): Unit =
      listeners.foreach(_.serializeModule(module))

    /** @inheritdoc */
    override def trackProgress(message: String, task: TaskProgress[_]): Unit =
      for (listener <- listeners) listener.trackProgress(message, task)

    /** @inheritdoc */
    override def startWaitingForResource(resource: Resource): Unit =
      for (listener <- listeners) listener.startWaitingForResource(resource)

    /** @inheritdoc */
    override def finishWaitingForResource(resource: Resource): Unit =
      for (listener <- listeners) listener.finishWaitingForResource(resource)

    /** Registers a new listener. */
    def addListener(listener: NotificationHandler): Unit =
      listeners ::= listener
  }
}
