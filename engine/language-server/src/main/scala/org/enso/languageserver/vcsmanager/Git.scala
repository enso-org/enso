package org.enso.languageserver.vcsmanager

import java.io.{FileNotFoundException, IOException}
import java.nio.file.{Files, Path}
import org.enso.languageserver.effect.BlockingIO
import org.eclipse.jgit.api.{Git => JGit}
import org.eclipse.jgit.api.ResetCommand.ResetType
import org.eclipse.jgit.api.errors.RefNotFoundException
import org.eclipse.jgit.errors.{
  IncorrectObjectTypeException,
  InvalidObjectIdException,
  MissingObjectException,
  RepositoryNotFoundException
}
import org.eclipse.jgit.lib.{ObjectId, Repository}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.util.SystemReader
import org.enso.languageserver.vcsmanager.Git.{
  AuthorEmail,
  AuthorName,
  MasterRef,
  RepoExists
}

import scala.jdk.CollectionConverters._
import zio.blocking.effectBlocking

import java.time.Instant
import scala.jdk.CollectionConverters.CollectionHasAsScala

private class Git(ensoDataDirectory: Option[Path]) extends VcsApi[BlockingIO] {

  private val gitDir = ensoDataDirectory
    .map(_.resolve(VcsApi.DefaultRepoDir))
    .getOrElse(Path.of(VcsApi.DefaultRepoDir))

  private def repository(root: Path): Repository = {
    val builder = new FileRepositoryBuilder()
    builder
      .setWorkTree(root.toFile)
      .setGitDir(root.resolve(gitDir).toFile)
      .setMustExist(true)
      .build()
  }

  override def init(root: Path): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val rootFile = root.toFile
      if (!rootFile.exists()) {
        throw new FileNotFoundException("unable to find project repo: " + root)
      }

      val repoLocation = root.resolve(gitDir)
      if (repoLocation.toFile.exists()) {
        throw new RepoExists()
      }

      if (!repoLocation.getParent.toFile.exists()) {
        if (!repoLocation.getParent.toFile.mkdirs()) {
          throw new FileNotFoundException(
            "unable to create project repository at " + repoLocation
          )
        }
      }

      val jgit = JGit
        .init()
        .setGitDir(repoLocation.toFile)
        .setDirectory(root.toFile)
        .setBare(false)
        .call()

      ensoDataDirectory.foreach { _ =>
        // When `gitDir` is set, JGit **always** creates a .git file (not a directory!) that points at the real
        // directory that has the repository.
        // The presence of the `.git` file is unnecessary and may be confusing to the users;
        // all operations specify explicitly the location of Git metadata directory.
        val dotGit = root.resolve(".git").toFile
        if (dotGit.exists() && dotGit.isFile) {
          dotGit.delete()
        }
      }

      val isDataDir =
        ensoDataDirectory.contains _
      val filesToAdd =
        listDirectoryFiles(root, Set(gitDir))
          .filterNot(isDataDir)
          .map(ensureUnixPathSeparator)
      if (filesToAdd.nonEmpty) {
        filesToAdd
          .foldLeft(jgit.add()) { case (cmd, filePath) =>
            cmd.addFilepattern(filePath)
          }
          .call()
      }

      jgit
        .commit()
        .setAllowEmpty(true)
        .setAll(true)
        .setMessage("Initial commit")
        .setAuthor(AuthorName, AuthorEmail)
        .call()
      ()
    }.mapError(errorHandling)
  }

  private def ensureUnixPathSeparator(path: Path): String =
    ensureUnixPathSeparator(path.toString)

  // AddCommand.addFilePattern always expects `/` separator
  private def ensureUnixPathSeparator(path: String): String =
    path.replaceAll("\\\\", "/")

  override def commit(
    root: Path,
    named: Option[String]
  ): BlockingIO[VcsFailure, RepoCommit] = {
    effectBlocking {
      val repo = repository(root)

      val commitName = named.getOrElse(Instant.now().toString)
      val jgit       = new JGit(repo)

      // Include files that already are/were in the index
      jgit
        .add()
        .addFilepattern(".")
        .setUpdate(true)
        .call()

      val status      = jgit.status.call()
      val untracked   = status.getUntracked().asScala.map(ensureUnixPathSeparator)
      val unixGitDir  = ensureUnixPathSeparator(gitDir)
      val unixDataDir = ensoDataDirectory.map(ensureUnixPathSeparator)
      val isDataDir =
        (x: String) => unixDataDir.map(dataDir => dataDir == x).getOrElse(false)
      val filesToAdd = untracked.flatMap { file =>
        if (!file.startsWith(unixGitDir) && !isDataDir(file)) {
          Some(file)
        } else None
      }
      if (!filesToAdd.isEmpty) {
        val addCmd = jgit.add()
        filesToAdd.foreach(filePath =>
          addCmd.addFilepattern(ensureUnixPathSeparator(filePath))
        )
        addCmd.call()
      }

      val revCommit = jgit
        .commit()
        .setMessage(commitName)
        .setAuthor(AuthorName, AuthorEmail)
        .call()
      RepoCommit(revCommit.getName(), revCommit.getShortMessage())
    }.mapError(errorHandling)
  }

  override def restore(
    root: Path,
    commitId: Option[String]
  ): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val repo = repository(root)

      val jgit = new JGit(repo)
      val resetCmd = jgit
        .reset()
        .setMode(ResetType.HARD)

      commitId match {
        case Some(name) =>
          val foundRev = findRevision(repo, name).getOrElse(
            throw new RefNotFoundException(name)
          )
          // Reset first to avoid checkout conflicts
          resetCmd.call()
          jgit
            .checkout()
            .setName(foundRev.getName)
            .call()
        case None =>
          resetCmd.call()
      }
      ()
    }.mapError(errorHandling)
  }

  private def findRevision(
    repo: Repository,
    sha: String
  ): Option[RevCommit] = {
    val walk = new RevWalk(repo)
    try {
      val revCommitId = ObjectId.fromString(sha)
      Some(walk.parseCommit(revCommitId))
    } catch {
      case _: MissingObjectException       => None
      case _: IncorrectObjectTypeException => None
      case _: InvalidObjectIdException     => None
      case _: IOException                  => None
    } finally {
      walk.dispose()
    }
  }

  override def status(root: Path): BlockingIO[VcsFailure, RepoStatus] = {
    effectBlocking {
      val repo       = repository(root)
      val jgit       = new JGit(repo)
      val statusCmd  = jgit.status()
      val status     = statusCmd.call()
      val unixGitDir = ensureUnixPathSeparator(gitDir)
      val changed =
        status.getModified().asScala.toList ++
        status
          .getUntracked()
          .asScala
          .toList
          .map(ensureUnixPathSeparator)
          .filterNot(_.startsWith(unixGitDir)) ++
        status.getRemoved().asScala.toList
      val changedPaths = changed.map(name => Path.of(name)).toSet
      val logCmd       = jgit.log()
      val last =
        Option(repo.resolve(MasterRef)) flatMap (_ => {
          val logs = logCmd.setMaxCount(1).call().iterator()
          if (logs.hasNext()) {
            val log = logs.next()
            Option(RepoCommit(log.getName, log.getShortMessage()))
          } else None
        }) getOrElse null
      RepoStatus(changed.nonEmpty, changedPaths, last)
    }.mapError(errorHandling)
  }

  override def list(
    root: Path,
    limit: Option[Int]
  ): BlockingIO[VcsFailure, List[RepoCommit]] = {
    effectBlocking {
      val jgit   = new JGit(repository(root))
      val logCmd = jgit.log()
      limit
        .filter(_ > 0)
        .map(logCmd.setMaxCount)
        .getOrElse(logCmd)
        .call()
        .asScala
        .toList
        .map(rev => RepoCommit(rev.getName, rev.getShortMessage))
    }.mapError(errorHandling)
  }

  private val errorHandling: Throwable => VcsFailure = {
    case ex: FileNotFoundException       => ProjectNotFound(ex.getMessage)
    case ex: RepositoryNotFoundException => RepoNotFound(ex.getMessage)
    case _: RefNotFoundException         => SaveNotFound
    case _: RepoExists                   => RepoAlreadyExists
    case ex                              => GenericVcsFailure(ex.getMessage)
  }

  private def listDirectoryFiles(
    rootDir: Path,
    excludeDirs: Set[Path]
  ): List[Path] = {
    listDirectoryFiles(rootDir, rootDir, excludeDirs)
  }
  private def listDirectoryFiles(
    rootDir: Path,
    dir: Path,
    excludeDirs: Set[Path]
  ): List[Path] = {
    val stream = Files.newDirectoryStream(dir)
    try {
      stream
        .iterator()
        .asScala
        .flatMap { absolutePath =>
          val relativePath = rootDir.relativize(absolutePath)
          if (absolutePath.toFile.isDirectory) {
            if (excludeDirs.forall(!relativePath.startsWith(_))) {
              relativePath :: listDirectoryFiles(
                rootDir,
                absolutePath,
                excludeDirs
              )
            } else Nil
          } else {
            List(relativePath)
          }
        }
        .toList
    } finally {
      if (stream != null) stream.close()
    }
  }
}

object Git {
  private val MasterRef   = "refs/heads/master"
  private val AuthorName  = "Enso VCS"
  private val AuthorEmail = "vcs@enso.io"

  private class RepoExists extends Exception

  /** Returns a Git implementation of VcsApi that ignores gitconfig file in
    * user's home directory.
    */
  def withEmptyUserConfig(
    dataDir: Option[Path]
  ): VcsApi[BlockingIO] = {
    SystemReader.setInstance(new EmptyUserConfigReader)
    new Git(dataDir)
  }
}
