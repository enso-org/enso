package org.enso.launcher.releases
import java.nio.file.Path

import org.enso.cli.{ProgressListener, TaskProgress}
import org.enso.launcher.FileSystem

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object FakeReleaseProvider extends ReleaseProvider {
  override def releaseForVersion(tag: String): Try[Release] = {
    if (tag == FakeEngine.tag) Success(FakeEngine)
    else if (tag == FakeGraal.tag) Success(FakeGraal)
    else Failure(new RuntimeException("unknown release"))
  }

  override def listReleases(): Try[Seq[Release]] = Success(Seq(FakeEngine))
}

object FakeGraal extends Release {
  override def tag: String = "vm-20.1.0"
  override def assets: Seq[Asset] =
    Seq(FakeAsset("graalvm-ce-java11-linux-amd64-20.1.0.tar.gz"))
}

object FakeEngine extends Release {
  override def tag: String = "enso-0.1.0"
  override def assets: Seq[Asset] =
    Seq(
      FakeAsset("manifest.yaml"),
      FakeAsset("enso-engine-0.1.0.zip")
    )
}

case class FakeAsset(sourcePath: String) extends Asset {
  private val source            = Path.of("fake_release/" + sourcePath)
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
