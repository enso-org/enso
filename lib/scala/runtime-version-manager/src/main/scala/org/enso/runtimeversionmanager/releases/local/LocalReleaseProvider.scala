package org.enso.runtimeversionmanager.releases.local

import java.nio.file.Path

import cats.syntax.traverse._
import com.typesafe.scalalogging.Logger
import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.FileSystem
import org.enso.runtimeversionmanager.releases.{
  Asset,
  Release,
  SimpleReleaseProvider
}

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
  *
  * It is given a `releaseDirectory` that should contain separate directories
  * for each local release. The name of each subdirectory corresponds to its
  * release tag and every file in that subdirectory is considered as an asset of
  * that release.
  *
  * It loads the list of releases at construction and thus may throw an error if
  * it cannot access the provided directory.
  */
class LocalReleaseProvider(
  releaseDirectory: Path,
  fallback: SimpleReleaseProvider
) extends SimpleReleaseProvider {
  private val logger = Logger[LocalReleaseProvider]
  private val localDirectories: Seq[Path] =
    FileSystem.listDirectory(releaseDirectory).filter { dir =>
      val isIgnoredFile =
        FileSystem.ignoredFileNames.contains(dir.getFileName.toString)
      !isIgnoredFile
    }

  /** @inheritdoc */
  override def releaseForTag(tag: String): Try[Release] = {
    val localPath = localDirectories.find(_.getFileName.toString == tag)
    localPath
      .map(wrapLocalDirectory)
      .getOrElse { fallback.releaseForTag(tag) }
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

  /** An asset that is on the local filesystem. */
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

  /** Creates a [[LocalRelease]] defined by a local directory. */
  private def wrapLocalDirectory(path: Path): Try[Release] = Try {
    val tag    = path.getFileName.toString
    val assets = FileSystem.listDirectory(path).map(LocalAsset)
    LocalRelease(tag, assets)
  }

  private def findLocalReleases(): Try[Seq[Release]] =
    localDirectories.map(wrapLocalDirectory).toList.sequence[Try, Release]
}
