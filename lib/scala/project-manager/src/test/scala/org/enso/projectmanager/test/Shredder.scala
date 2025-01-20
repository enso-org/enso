package org.enso.projectmanager.test

import org.apache.commons.io.FileUtils
import org.enso.projectmanager.infrastructure.desktop.TrashCan
import zio.ZIO

import java.io.File

class Shredder[R] extends TrashCan[ZIO[R, +*, +*]] {

  /** @inheritdoc */
  override def moveToTrash(path: File): ZIO[R, Nothing, Boolean] =
    ZIO
      .attemptBlocking(FileUtils.forceDelete(path))
      .fold(
        failure = _ => false,
        success = _ => true
      )
}
