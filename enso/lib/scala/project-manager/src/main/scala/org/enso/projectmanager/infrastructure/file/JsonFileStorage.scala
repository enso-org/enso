package org.enso.projectmanager.infrastructure.file

import java.io.File
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.FileStorage._
import shapeless.{:+:, CNil, _}

import scala.annotation.nowarn

/** ZIO implementation of [[FileStorage]]. It uses circe [[Encoder]] and
  * [[Decoder]] to encode/decode objects.
  *
  * @param path a path to a file that stores serialized object
  * @param fileSystem a filesystem algebra
  * @tparam A a datatype to store
  */
@nowarn("msg=parameter value evidence")
class JsonFileStorage[
  A: Encoder: Decoder,
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap
](
  path: File,
  fileSystem: FileSystem[F]
) extends FileStorage[A, F] {

  /** Loads the serialized object from the file.
    *
    * @return either [[LoadFailure]] or the object
    */
  override def load(): F[LoadFailure, A] =
    fileSystem
      .readFile(path)
      .mapError(Coproduct[LoadFailure](_))
      .flatMap(tryDecodeFileContents)

  private def tryDecodeFileContents(contents: String): F[LoadFailure, A] = {
    decode[A](contents) match {
      case Left(failure) =>
        ErrorChannel[F].fail(
          Coproduct[LoadFailure](CannotDecodeData(failure.getMessage))
        )

      case Right(value) => CovariantFlatMap[F].pure(value)
    }
  }

  /** Persists the provided object on the disk.
    *
    * @param data a data object
    * @return either [[FileSystemFailure]] or success
    */
  override def persist(data: A): F[FileSystemFailure, Unit] =
    fileSystem.overwriteFile(path, data.asJson.spaces2)

  /** Atomically modifies persisted object using function `f`.
    *
    * @param f the update function that takes the current version of the object
    *          loaded from the disk and returns a tuple containing the new
    *          version of object 3and value to return
    * @tparam B a type of returned value
    * @return either [[LoadFailure]] or the result of updating object
    */
  override def modify[B](
    f: A => (A, B)
  ): F[CannotDecodeData :+: FileSystemFailure :+: CNil, B] =
    // format: off
    for {
      index             <- load()
      (updated, output)  = f(index)
      _                 <- persist(updated).mapError(Coproduct[LoadFailure](_))
    } yield output
    // format: on

}
