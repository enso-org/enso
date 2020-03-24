package org.enso.projectmanager.infrastructure.file

import org.enso.projectmanager.infrastructure.file.FileStorage._
import shapeless._

/**
  * An abstraction for storing data object in the file.
  *
  * @tparam A a datatype to store
  * @tparam F a monadic context
  */
trait FileStorage[A, F[+_, +_]] {

  /**
    * Loads the serialized object from the file.
    *
    * @return either [[LoadFailure]] or the object
    */
  def load(): F[LoadFailure, A]

  /**
    * Persists the provided object on the disk.
    *
    * @param data a data object
    * @return either [[FileSystemFailure]] or success
    */
  def persist(data: A): F[FileSystemFailure, Unit]

  /**
    * Atomically modifies persisted object using function `f`.
    *
    * @param f the update function that takes the current version of the object
    *          loaded from the disk and returns a tuple containing the new
    *          version of object 3and value to return
    * @tparam B a type of returned value
    * @return either [[LoadFailure]] or the result of updating object
    */
  def modify[B](f: A => (A, B)): F[LoadFailure, B]

}

object FileStorage {

  /**
    * Signals that representation
    *
    * @param msg an error message
    */
  case class CannotDecodeData(msg: String)

  /**
    * A failure type for load operation.
    */
  type LoadFailure = CannotDecodeData :+: FileSystemFailure :+: CNil

}
