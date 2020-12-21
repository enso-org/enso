package org.enso.runtimeversionmanager.releases.testing

import java.nio.file.{Files, Path, StandardCopyOption}

import org.enso.cli.task.{ProgressListener, TaskProgress}
import org.enso.runtimeversionmanager.FileSystem
import org.enso.runtimeversionmanager.locking.{LockManager, LockType}
import org.enso.runtimeversionmanager.releases.{
  Asset,
  Release,
  ReleaseProviderException,
  SimpleReleaseProvider
}

import scala.io.Source
import scala.util.{Success, Try, Using}

/** A release provider that creates fake releases from the specified files.
  *
  * @param releasesRoot path to the directory containing subdirectories for each
  *                     release
  * @param copyIntoArchiveRoot list of filenames that will be copied to the root
  *                            of each created archive
  * @param lockManagerForAssets if the test wants to wait on locks for assets,
  *                             it should provide a lock manager to handle that
  */
case class FakeReleaseProvider(
  releasesRoot: Path,
  copyIntoArchiveRoot: Seq[String]          = Seq.empty,
  lockManagerForAssets: Option[LockManager] = None
) extends SimpleReleaseProvider {

  private val releases =
    FileSystem
      .listDirectory(releasesRoot)
      .map(FakeRelease(_, copyIntoArchiveRoot, lockManagerForAssets))

  /** @inheritdoc */
  override def releaseForTag(tag: String): Try[Release] =
    releases
      .find(_.tag == tag)
      .toRight(ReleaseProviderException(s"Release $tag does not exist."))
      .toTry

  /** @inheritdoc */
  override def listReleases(): Try[Seq[Release]] = Success(releases)
}

/** The release created by [[FakeReleaseProvider]].
  *
  * @param path path to the release root, each file or directory inside of it
  *             represents a [[FakeAsset]]
  */
case class FakeRelease(
  path: Path,
  copyIntoArchiveRoot: Seq[String]          = Seq.empty,
  lockManagerForAssets: Option[LockManager] = None
) extends Release {

  /** @inheritdoc
    */
  override def tag: String = path.getFileName.toString

  /** @inheritdoc
    */
  override def assets: Seq[Asset] = {
    val pathsToCopy = copyIntoArchiveRoot.map(path.resolve)
    FileSystem
      .listDirectory(path)
      .map(FakeAsset(_, pathsToCopy, lockManagerForAssets))
  }
}

/** Represents an asset of the [[FakeRelease]].
  *
  * If it is a file, 'downloading' it just copies it to the destination.
  * Fetching it reads it as text.
  *
  * If it is a directory, it is treated as a fake archive - an archive with all
  * files contained within that directory is created at the specified
  * destination. Any paths from `copyIntoArchiveRoot` are also added into the
  * root of that created archive. This allows to avoid maintaining additional
  * copies of shared files like the manifest.
  */
case class FakeAsset(
  source: Path,
  pathsToCopy: Seq[Path],
  lockManagerForAssets: Option[LockManager] = None
) extends Asset {

  /** @inheritdoc
    */
  override def fileName: String = source.getFileName.toString

  /** @inheritdoc */
  override def downloadTo(path: Path): TaskProgress[Unit] = {
    maybeWaitForAsset()
    val result = Try(copyFakeAsset(path))
    new TaskProgress[Unit] {
      override def addProgressListener(
        listener: ProgressListener[Unit]
      ): Unit = {
        listener.done(result)
      }
    }
  }

  /** If [[lockManagerForAssets]] is set, acquires a shared lock on the asset.
    *
    * The test runner may grab an exclusive lock on an asset as a way to
    * synchronize actions (this download will wait until such exclusive lock is
    * released).
    */
  private def maybeWaitForAsset(): Unit =
    lockManagerForAssets.foreach { lockManager =>
      val name     = "testasset-" + fileName
      val lockType = LockType.Shared
      val lock = lockManager.tryAcquireLock(
        name,
        lockType
      ) match {
        case Some(immediateLock) =>
          System.err.println(s"[TEST] Lock for $name was acquired immediately.")
          immediateLock
        case None =>
          System.err.println("INTERNAL-TEST-ACQUIRING-LOCK")
          lockManager.acquireLock(name, lockType)
      }
      lock.release()
    }

  private def copyFakeAsset(destination: Path): Unit =
    if (Files.isDirectory(source))
      copyArchive(destination)
    else
      copyNormalFile(destination)

  private def copyArchive(destination: Path): Unit = {
    lazy val innerRoot = {
      val roots = FileSystem.listDirectory(source).filter(Files.isDirectory(_))
      if (roots.length > 1) {
        throw new IllegalStateException(
          "Cannot copy files into the root if there are more than one root."
        )
      }

      roots.headOption.getOrElse(source)
    }

    for (sourceToCopy <- pathsToCopy) {
      Files.copy(
        sourceToCopy,
        innerRoot.resolve(sourceToCopy.getFileName),
        StandardCopyOption.REPLACE_EXISTING
      )
    }
    TestArchivePackager.packArchive(source, destination)
  }

  private def copyNormalFile(destination: Path): Unit =
    FileSystem.copyFile(source, destination)

  /** @inheritdoc */
  override def fetchAsText(): TaskProgress[String] =
    if (Files.isDirectory(source))
      throw new IllegalStateException(
        "Cannot fetch a fake archive (a directory) as text."
      )
    else {
      maybeWaitForAsset()
      val txt = Using(Source.fromFile(source.toFile)) { src =>
        src.getLines().mkString("\n")
      }
      (listener: ProgressListener[String]) => {
        listener.done(txt)
      }
    }
}
