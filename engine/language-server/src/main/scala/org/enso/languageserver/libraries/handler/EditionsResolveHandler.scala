package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.editions.EditionResolver
import org.enso.jsonrpc.{Request, ResponseError}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.EditionReferenceResolver
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.util.UnhandledLogging

import scala.util.{Failure, Success}

class EditionsResolveHandler(
  editionReferenceResolver: EditionReferenceResolver,
  editionResolver: EditionResolver
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = {
    case Request(EditionsResolve, id, EditionsResolve.Params(reference)) =>
      val result = for {
        rawEdition      <- editionReferenceResolver.resolveReference(reference)
        resolvedEdition <- editionResolver.resolve(rawEdition).toTry
      } yield resolvedEdition.getEngineVersion

      result match {
        case Failure(exception) =>
        case Success(value)     =>
      }
      // TODO [RW] actual implementation
      sender() ! ResponseError(
        Some(id),
        FileSystemError("Feature not implemented")
      )
  }
}

object EditionsResolveHandler {
  def props(
    editionReferenceResolver: EditionReferenceResolver,
    editionResolver: EditionResolver
  ): Props = Props(new EditionsResolveHandler())
}
