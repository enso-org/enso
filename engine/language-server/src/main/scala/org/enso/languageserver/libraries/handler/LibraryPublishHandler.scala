package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.cli.task.notifications.ActorProgressNotificationForwarder
import org.enso.jsonrpc.{Request, ResponseError, ResponseResult, Unused}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.util.UnhandledLogging
import org.enso.libraryupload.{auth, LibraryUploader}

import scala.util.{Failure, Success}

/** A request handler for the `library/publish` endpoint.
  *
  * It is currently a stub implementation which will be refined later on.
  */
class LibraryPublishHandler
    extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = {
    case Request(
          LibraryPublish,
          id,
          LibraryPublish.Params(
            namespace,
            name,
            authToken,
            uploadUrl,
            bumpVersionAfterPublish
          )
        ) =>
      import context.dispatcher

      def libraryRoot = ??? // TODO find library

      val _     = (name, namespace, bumpVersionAfterPublish) // TODO
      val token = auth.SimpleHeaderToken(authToken)

      val progressReporter =
        ActorProgressNotificationForwarder.translateAndForward(
          LibraryPublish.name,
          sender()
        )
      val result = LibraryUploader.uploadLibrary(
        libraryRoot,
        uploadUrl,
        token,
        progressReporter
      )

      result match {
        case Failure(exception) =>
          sender() ! ResponseError(
            Some(id),
            FileSystemError(s"Upload failed: $exception")
          )
        case Success(_) =>
          sender() ! ResponseResult(LibraryPublish, id, Unused)
      }
  }
}

object LibraryPublishHandler {

  /** Creates a configuration object to create [[LibraryPublishHandler]]. */
  def props(): Props = Props(new LibraryPublishHandler)
}
