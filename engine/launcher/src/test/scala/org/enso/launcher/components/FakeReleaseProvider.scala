package org.enso.launcher.components

import java.nio.file.{Files, Path}

import org.enso.cli.{ProgressListener, TaskProgress}
import org.enso.launcher.{FileSystem, OS}
import org.enso.launcher.releases.{Asset, Release, ReleaseProvider}

import scala.io.Source
import scala.util.{Success, Try, Using}
import sys.process._

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
    val result = Try(copyFakeAsset(path))
    new TaskProgress[Unit] {
      override def addProgressListener(
        listener: ProgressListener[Unit]
      ): Unit = {
        listener.done(result)
      }
    }
  }

  private def copyFakeAsset(destination: Path): Unit =
    if (Files.isDirectory(source)) {
      val directoryName = source.getFileName.toString
      if (directoryName.endsWith(".tar.gz") && OS.isUNIX)
        packTarGz(source, destination)
      else if (directoryName.endsWith(".zip") && OS.isWindows)
        packZip(source, destination)
      else {
        throw new IllegalArgumentException(
          s"Fake-archive format $directoryName is not supported on " +
          s"${OS.operatingSystem}."
        )
      }
    } else {
      FileSystem.copyFile(source, destination)
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

  private def packTarGz(source: Path, destination: Path): Unit = {
    val files = FileSystem.listDirectory(source)
    val exitCode = Process(
      Seq(
        "tar",
        "-czf",
        destination.toAbsolutePath.toString
      ) ++ files.map(_.getFileName.toString),
      source.toFile
    ).!
    if (exitCode != 0) {
      throw new RuntimeException(
        s"tar failed. Cannot create fake-archive for $source"
      )
    }
  }

  private def packZip(source: Path, destination: Path): Unit = {
    val files = FileSystem.listDirectory(source)
    val exitCode = Process(
      Seq(
        "powershell",
        "Compress-Archive",
        "-Path",
        files.map(_.getFileName.toString).mkString(","),
        "-DestinationPath",
        destination.toAbsolutePath.toString
      ),
      source.toFile
    ).!
    if (exitCode != 0) {
      throw new RuntimeException(
        s"tar failed. Cannot create fake-archive for $source"
      )
    }
  }
}
