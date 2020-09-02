package org.enso.projectmanager.infrastructure.file

import java.io.File

import org.enso.pkg.{Package, PackageManager}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFailure
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFailure.{
  InconsistentStorage,
  StorageFailure
}

final class ProjectPackageStorage[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap
](directory: File) {

  def load: F[ProjectRepositoryFailure, Option[Package[File]]] =
    Sync[F]
      .blockingOp { PackageManager.Default.fromDirectory(directory) }
      .mapError(th => StorageFailure(th.toString))

  def rename(name: String): F[ProjectRepositoryFailure, Unit] =
    load
      .flatMap {
        case None =>
          ErrorChannel[F].fail(
            InconsistentStorage(s"Cannot find package.yaml at $directory")
          )
        case Some(projectPackage) => CovariantFlatMap[F].pure(projectPackage)
      }
      .flatMap { projectPackage =>
        val newName = PackageManager.Default.normalizeName(name)
        Sync[F]
          .blockingOp { projectPackage.rename(newName) }
          .map(_ => ())
          .mapError(th => StorageFailure(th.toString))
      }

}
