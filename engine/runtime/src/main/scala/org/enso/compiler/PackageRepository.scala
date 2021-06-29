package org.enso.compiler

import org.enso.editions.LibraryName
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.librarymanager.LibraryProvider

import scala.jdk.CollectionConverters._

trait PackageRepository {

  /** Informs the repository that it should populate the top scope with modules
    * belonging to a given package.
    *
    * @param namespace the namespace of the package.
    * @param name the package name.
    * @return `Right(())` if the package was already loaded or successfully
    *         downloaded. A `Left` containing an error otherwise.
    */
  def ensurePackageIsLoaded(
    libraryName: LibraryName
  ): Either[PackageRepository.Error, Unit]
}

object PackageRepository {

  /** A trait representing errors reported by this system */
  sealed trait Error

  object Error {

    /** An error reported when the requested package does not exist.
      */
    case object PackageDoesNotExist extends Error
  }

  /** A temporary package repository, only able to resolve packages known
    * upfront to the language context.
    *
    * @param context the language context
    */
  class Legacy(context: Context) extends PackageRepository {

    /** @inheritdoc */
    override def ensurePackageIsLoaded(
      libraryName: LibraryName
    ): Either[PackageRepository.Error, Unit] =
      if (
        (libraryName.name == Builtins.PACKAGE_NAME && libraryName.prefix == Builtins.NAMESPACE) ||
        context.getPackages.asScala
          .exists(p =>
            p.name == libraryName.name && p.namespace == libraryName.prefix
          )
      ) Right(())
      else Left(Error.PackageDoesNotExist)
  }

  class Default(libraryProvider: LibraryProvider, context: Context)
      extends PackageRepository {
    override def ensurePackageIsLoaded(
      libraryName: LibraryName
    ): Either[Error, Unit] = ???
  }
}
