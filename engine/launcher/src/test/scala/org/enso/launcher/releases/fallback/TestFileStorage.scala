package org.enso.launcher.releases.fallback

import java.nio.file.Path

import org.enso.cli.task.{TaskProgress, TaskProgressImplementation}
import org.enso.runtimeversionmanager.FileSystem
import org.enso.launcher.TestHelpers
import org.enso.launcher.releases.fallback.staticwebsite.FileStorage

import scala.util.Try

class TestFileStorage(root: Path) extends FileStorage {
  override def download(
    path: Seq[String],
    destination: Path
  ): TaskProgress[Unit] =
    doInBackground {
      FileSystem.copyFile(resolve(path), destination)
    }

  override def fetchString(path: Seq[String]): TaskProgress[String] =
    doInBackground {
      TestHelpers.readFileContent(resolve(path))
    }

  private def resolve(path: Seq[String]): Path =
    path.foldLeft(root)(_.resolve(_))

  private def doInBackground[R](action: => R): TaskProgress[R] = {
    val impl   = new TaskProgressImplementation[R]
    val thread = new Thread(() => impl.setComplete(Try(action)))
    thread.start()
    impl
  }
}
