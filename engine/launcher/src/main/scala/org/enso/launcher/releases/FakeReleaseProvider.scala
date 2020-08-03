package org.enso.launcher.releases
import java.nio.file.Path

import org.enso.cli.{ProgressListener, TaskProgress}
import org.enso.launcher.FileSystem

import scala.io.Source
import scala.util.{Success, Try, Using}

case class FakeReleaseProvider(releasesRoot: Path) extends ReleaseProvider {
  private val releases = FileSystem.listDirectory(releasesRoot).map(FakeRelease)

  override def releaseForVersion(tag: String): Try[Release] =
    releases
      .find(_.tag == tag)
      .toRight(new RuntimeException("unknown release"))
      .toTry

  override def listReleases(): Try[Seq[Release]] = Success(releases)
}

case class FakeRelease(path: Path) extends Release {
  override def tag: String = path.getFileName.toString
  override def assets: Seq[Asset] =
    FileSystem.listDirectory(path).map(FakeAsset)
}

case class FakeAsset(source: Path) extends Asset {
  override def fileName: String = source.getFileName.toString

  override def downloadTo(path: Path): TaskProgress[Unit] = {
    FileSystem.copyFile(source, path)
    new TaskProgress[Unit] {
      override def addProgressListener(
        listener: ProgressListener[Unit]
      ): Unit = {
        listener.done(Success(()))
      }
    }
  }

  override def fetchAsText(): TaskProgress[String] = {
    val txt = Using(Source.fromFile(source.toFile)) { src =>
      src.getLines().mkString("\n")
    }
    new TaskProgress[String] {
      override def addProgressListener(
        listener: ProgressListener[String]
      ): Unit = {
        listener.done(txt)
      }
    }
  }
}
