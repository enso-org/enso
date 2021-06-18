package org.enso.editions.provider

import org.enso.editions.{EditionSerialization, Editions}

import java.io.FileNotFoundException
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/** An implementation of [[EditionProvider]] that looks for the edition files in
  *  a list of filesystem paths.
  */
case class FileSystemEditionProvider(searchPaths: List[Path])
    extends EditionProvider {

  /** @inheritdoc */
  override def findEditionForName(name: String): Try[Editions.Raw.Edition] = {
    val result = findEdition(name, searchPaths)
    result match {
      case Left(EditionNotFound) =>
        Failure(new FileNotFoundException(s"Could not find edition `$name`."))
      case Left(EditionReadError(error)) => Failure(error)
      case Right(value)                  => Success(value)
    }
  }

  private val editionSuffix = ".yaml"

  @tailrec
  private def findEdition(
    name: String,
    paths: List[Path]
  ): Either[EditionLoadingError, Editions.Raw.Edition] = paths match {
    case head :: tail =>
      val headResult = loadEdition(name, head)
      headResult match {
        case Left(EditionNotFound) =>
          findEdition(name, tail)
        case _ => headResult
      }
    case Nil => Left(EditionNotFound)
  }

  sealed private trait EditionLoadingError
  private case object EditionNotFound extends EditionLoadingError
  private case class EditionReadError(error: Throwable)
      extends EditionLoadingError

  private def loadEdition(
    name: String,
    path: Path
  ): Either[EditionLoadingError, Editions.Raw.Edition] = {
    val fileName    = name + editionSuffix
    val editionPath = path.resolve(fileName)
    if (Files.exists(editionPath)) {
      EditionSerialization
        .loadEdition(editionPath)
        .toEither
        .left
        .map(EditionReadError)
    } else Left(EditionNotFound)
  }
}
