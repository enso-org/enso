package org.enso.projectmanager.infrastructure

import org.enso.projectmanager.infrastructure.file.FileStorage.CannotDecodeData
import org.enso.projectmanager.infrastructure.file.FileSystemFailure
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFailure.{
  CannotLoadIndex,
  StorageFailure
}
import shapeless._

package object repository {

  /**
    * Polymorphic function converting [[org.enso.projectmanager.infrastructure.file.FileStorage]]
    * failures to [[ProjectRepositoryFailure]]
    */
  object convertFileStorageFailure extends Poly1 {

    implicit def caseCannotDecodeData =
      at[CannotDecodeData](f => CannotLoadIndex(f.msg))

    implicit def caseFsFailure =
      at[FileSystemFailure](f => StorageFailure(s"IO operation failed: $f"))
  }

}
