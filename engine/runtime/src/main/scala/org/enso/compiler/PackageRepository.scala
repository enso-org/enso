package org.enso.compiler

import org.enso.interpreter.runtime.Context
import org.enso.interpreter.runtime.builtin.Builtins

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
    namespace: String,
    name: String
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
  class Default(context: Context) extends PackageRepository {

    /** @inheritdoc */
    override def ensurePackageIsLoaded(
      namespace: String,
      name: String
    ): Either[PackageRepository.Error, Unit] =
      if (
        (name == Builtins.PACKAGE_NAME && namespace == Builtins.NAMESPACE) ||
        context.getPackages.asScala
          .exists(p => p.name == name && p.namespace == namespace)
      ) Right(())
      else Left(Error.PackageDoesNotExist)
  }
}
