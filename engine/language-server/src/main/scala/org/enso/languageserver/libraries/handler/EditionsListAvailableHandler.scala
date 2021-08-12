package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.editions.updater.EditionManager
import org.enso.jsonrpc.{Request, ResponseError, ResponseResult}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.util.UnhandledLogging

import scala.util.{Failure, Success, Try}

/** A request handler for the `editions/listAvailable` endpoint.
  *
  * @param editionManager an edition manager instance
  */
class EditionsListAvailableHandler(editionManager: EditionManager)
    extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = {
    case Request(
          EditionsListAvailable,
          id,
          EditionsListAvailable.Params(update)
        ) =>
      Try(editionManager.findAllAvailableEditions(update)) match {
        case Success(editions) =>
          sender() ! ResponseResult(
            EditionsListAvailable,
            id,
            EditionsListAvailable.Result(editions.sorted)
          )
        case Failure(exception) =>
          sender() ! ResponseError(
            Some(id),
            FileSystemError(exception.toString)
          )
      }
  }
}

object EditionsListAvailableHandler {

  /** Creates a configuration object to create [[EditionsListAvailableHandler]].
    *
    * @param editionManager an edition manager instance
    */
  def props(editionManager: EditionManager): Props = Props(
    new EditionsListAvailableHandler(editionManager)
  )
}
