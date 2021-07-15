package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{Request, ResponseError, ResponseResult}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.EditionReferenceResolver
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.util.UnhandledLogging

import scala.util.{Failure, Success}

class EditionsResolveHandler(editionReferenceResolver: EditionReferenceResolver)
    extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = {
    case Request(EditionsResolve, id, EditionsResolve.Params(reference)) =>
      val result = for {
        edition <- editionReferenceResolver.resolveEdition(reference)
      } yield edition.getEngineVersion

      result match {
        case Failure(exception) =>
          // TODO [RW] more detailed errors
          sender() ! ResponseError(
            Some(id),
            FileSystemError(exception.getMessage)
          )
        case Success(engineVersion) =>
          sender() ! ResponseResult(
            EditionsResolve,
            id,
            EditionsResolve.Result(engineVersion.toString)
          )
      }
  }
}

object EditionsResolveHandler {
  def props(editionReferenceResolver: EditionReferenceResolver): Props = Props(
    new EditionsResolveHandler(editionReferenceResolver)
  )
}
