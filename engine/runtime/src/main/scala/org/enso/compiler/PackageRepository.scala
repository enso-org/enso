package org.enso.compiler

import org.enso.editions.LibraryName
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.librarymanager.ResolvingLibraryProvider
import org.enso.librarymanager.ResolvingLibraryProvider.Error

import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success}

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
    case class PackageCouldNotBeResolved(cause: Throwable) extends Error
    case class PackageDownloadFailed(cause: Throwable)     extends Error
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
      else Left(Error.PackageCouldNotBeResolved(new NotImplementedError))
  }

  class Default(libraryProvider: ResolvingLibraryProvider, context: Context)
      extends PackageRepository {

    // TODO [RW, MK] what are the concurrency guarantees here? do we have only one thread or more?
    val loadedModules: collection.mutable.Set[LibraryName] = {
      val builtins = LibraryName(Builtins.NAMESPACE, Builtins.PACKAGE_NAME)
      collection.mutable.Set(builtins)
    }

    private def loadPackage(libraryName: LibraryName, root: Path): Unit = {
      loadedModules.add(libraryName)

      // TODO actually load stuff
    }

    override def ensurePackageIsLoaded(
      libraryName: LibraryName
    ): Either[Error, Unit] = if (loadedModules.contains(libraryName)) Right(())
    else
      libraryProvider
        .findLibrary(libraryName)
        .map(loadPackage(libraryName, _))
        .left
        .map {
          case ResolvingLibraryProvider.Error.NotResolved(details) =>
            Error.PackageCouldNotBeResolved(details)
          case ResolvingLibraryProvider.Error.DownloadFailed(reason) =>
            Error.PackageDownloadFailed(reason)
        }
  }
}
