package org.enso.runtimeversionmanager.releases.local

import java.nio.file.Path

import com.typesafe.scalalogging.Logger
import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.FileSystem
import org.enso.runtimeversionmanager.releases.{
  Asset,
  Release,
  SimpleReleaseProvider
}
import cats.syntax.traverse._

import scala.io.Source
import scala.util.{Try, Using}

/** A [[SimpleReleaseProvider]] that uses a repository located on a local file
  * system as its primary source and falls back to some other specified
  * repository in other cases.
  *
  * It can be used to implement bundling some versions with an installer - it
  * can point to this local repository, so that any required bundled versions
  * are installed from the bundle and any other versions are handled using the
  * default repository.
  */
class LocalReleaseProvider(
  releaseDirectory: Path,
  fallback: SimpleReleaseProvider
) extends SimpleReleaseProvider {
  private val logger = Logger[LocalReleaseProvider]

  /** @inheritdoc */
  override def releaseForTag(tag: String): Try[Release] = {
    localDirectories.flatMap { directories =>
      val localPath = directories.find(_.getFileName.toString == tag)
      localPath
        .map(wrapLocalDirectory)
        .getOrElse { fallback.releaseForTag(tag) }
    }
  }

  /** @inheritdoc */
  override def listReleases(): Try[Seq[Release]] = {
    val remote = fallback
      .listReleases()
      .recover { error =>
        logger.warn(
          s"The remote release provider failed with $error, but " +
          s"locally bundled releases are available."
        )
        Seq.empty
      }
      .get
    findLocalReleases() map { local =>
      val localTags = local.map(_.tag).toSet
      val remoteDeduplicated = remote.filter { remoteRelease =>
        val hasLocalCorrespondent = localTags.contains(remoteRelease.tag)
        !hasLocalCorrespondent
      }
      local ++ remoteDeduplicated
    }
  }

  private def localDirectories: Try[Seq[Path]] = Try {
    FileSystem.listDirectory(releaseDirectory)
  }

  private case class LocalAsset(assetPath: Path) extends Asset {

    /** @inheritdoc */
    override def fileName: String = assetPath.getFileName.toString

    /** @inheritdoc */
    override def downloadTo(path: Path): TaskProgress[Unit] =
      TaskProgress.runImmediately {
        FileSystem.copyFile(assetPath, path)
      }

    /** @inheritdoc */
    override def fetchAsText(): TaskProgress[String] =
      TaskProgress.fromTry {
        Using(Source.fromFile(assetPath.toFile)) { src =>
          src.getLines().mkString("\n")
        }
      }
  }

  private case class LocalRelease(
    override val tag: String,
    override val assets: Seq[LocalAsset]
  ) extends Release

  private def wrapLocalDirectory(path: Path): Try[Release] = Try {
    val tag    = path.getFileName.toString
    val assets = FileSystem.listDirectory(path).map(LocalAsset)
    LocalRelease(tag, assets)
  }

  private def findLocalReleases(): Try[Seq[Release]] =
    localDirectories.flatMap { directories =>
      directories.map(wrapLocalDirectory).toList.sequence
    }
}
