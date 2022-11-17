package org.enso.languageserver.vcsmanager

import org.apache.commons.io.FileUtils
import org.enso.languageserver.effect.Effects
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.eclipse.jgit.api.{Git => JGit}
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.revwalk.RevCommit

import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets

class GitSpec extends AnyWordSpecLike with Matchers with Effects {

  "VCS initialization" should {
    "create a new repository" in new TestCtx {
      val targetRepo = repoPath.resolve(".git")
      targetRepo.toFile shouldNot exist
      val result = vcs.init(repoPath).unsafeRunSync()
      result.isRight shouldBe true
      targetRepo.toFile should exist
    }

    "fail to create a repo for non-existent project" in new TestCtx {
      val path   = repoPath.resolve("foo")
      val result = vcs.init(path).unsafeRunSync()
      result.isLeft shouldBe true
      result.swap.getOrElse(null) shouldBe an[ProjectNotFound]

      val targetRepo = path.resolve(".git")
      targetRepo.toFile shouldNot exist
    }

    "fail to create a repo for a project that is already under vcs" in new TestCtx
      with InitialRepoSetup {
      val targetRepo = repoPath.resolve(".git")
      targetRepo.toFile should exist
      val result = vcs.init(repoPath).unsafeRunSync()
      result.isLeft shouldBe true
      result.swap.getOrElse(null) shouldBe an[RepoAlreadyExists.type]
      targetRepo.toFile should exist
    }
  }

  "VCS save" should {
    "commit to the repo" in new TestCtx with InitialRepoSetup {
      createStubFile(repoPath.resolve("Foo.enso")) should equal(true)

      val commitResult1 = vcs.commit(repoPath, "First").unsafeRunSync()
      commitResult1.isRight shouldBe true

      createStubFile(repoPath.resolve("Bar.enso")) should equal(true)
      val commitResult2 = vcs.commit(repoPath, "Second").unsafeRunSync()
      commitResult2.isRight shouldBe true

      val revisions = listCommits(repoPath)
      revisions should have length 3
      revisions(0).getFullMessage() should equal("Second")
      revisions(1).getFullMessage() should equal("First")
      revisions(2).getFullMessage() should equal("Initial commit")
    }

    "accept empty commits to the repo" in new TestCtx with InitialRepoSetup {
      val commitNames = List("First", "Second")
      commitNames.foreach { name =>
        val commitResult1 = vcs.commit(repoPath, name).unsafeRunSync()
        commitResult1.isRight shouldBe true
      }

      val revisions = listCommits(repoPath)
      revisions should have length 3
      revisions.map(_.getFullMessage()).reverse should equal(
        "Initial commit" :: commitNames
      )
    }

    "reject commit to the non-initialized repo" in new TestCtx {
      val path         = repoPath
      val commitResult = vcs.commit(path, "First").unsafeRunSync()
      commitResult.isLeft shouldBe true

      commitResult.swap.getOrElse(null) shouldBe an[RepoNotFound]
    }

    "commit without explicit name" in new TestCtx {
      val path       = repoPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      (0 to 2).foreach { _ =>
        val commitResult1 = vcs.commit(path, None).unsafeRunSync()
        commitResult1.isRight shouldBe true
      }

      val revisions = listCommits(path)
      revisions should have length 4
    }
  }

  "VCS status" should {
    "report dirty status when untracked files are added" in new TestCtx
      with InitialRepoSetup {
      val modifiedResultEither1 = vcs.status(repoPath).unsafeRunSync()
      modifiedResultEither1.isRight shouldBe true
      val r = modifiedResultEither1.getOrElse(null)
      r shouldBe an[RepoStatus]
      repoStatusIgnoreSha(r) should equal(
        RepoStatus(false, Set(), RepoCommit(null, "Initial commit"))
      )

      createStubFile(repoPath.resolve("Foo.enso")) should equal(true)

      val modifiedResult2 = vcs.status(repoPath).unsafeRunSync()
      modifiedResult2.isRight shouldBe true
      modifiedResult2.map(_.isDirty).getOrElse(null) shouldBe true
    }

    "not report dirty status when untracked files were committed" in new TestCtx
      with InitialRepoSetup {
      val modifiedResultEither1 = vcs.status(repoPath).unsafeRunSync()
      modifiedResultEither1.isRight shouldBe true
      val r = modifiedResultEither1.getOrElse(null)
      r shouldBe an[RepoStatus]
      repoStatusIgnoreSha(r) should equal(
        RepoStatus(false, Set(), RepoCommit(null, "Initial commit"))
      )

      createStubFile(repoPath.resolve("Foo.enso")) should equal(true)
      createStubFile(repoPath.resolve("Bar.enso")) should equal(true)
      val commitResult = vcs.commit(repoPath, "New files").unsafeRunSync()
      commitResult.isRight shouldBe true

      val modifiedResultEither2 = vcs.status(repoPath).unsafeRunSync()
      modifiedResultEither2.isRight shouldBe true
      val r2 = modifiedResultEither2.getOrElse(null)
      r2 shouldBe an[RepoStatus]
      repoStatusIgnoreSha(r2) shouldBe RepoStatus(
        false,
        Set(),
        RepoCommit(null, "New files")
      )
    }

    "report dirty status when tracked files were modified" in new TestCtx
      with InitialRepoSetup {
      val fooFile = repoPath.resolve("Foo.enso")
      createStubFile(fooFile) should equal(true)
      val commitResult = vcs.commit(repoPath, "New files").unsafeRunSync()
      commitResult.isRight shouldBe true

      Files.write(
        fooFile,
        "file contents".getBytes(StandardCharsets.UTF_8)
      )

      val modifiedResultEither2 = vcs.status(repoPath).unsafeRunSync()
      modifiedResultEither2.isRight shouldBe true
      val r = modifiedResultEither2.getOrElse(null)
      r shouldBe an[RepoStatus]
      repoStatusIgnoreSha(r) shouldBe RepoStatus(
        true,
        Set(fooFile.getFileName),
        RepoCommit(null, "New files")
      )

    }
  }

  "VCS restore" should {

    "reset to last saved state" in new TestCtx with InitialRepoSetup {
      val fooFile = repoPath.resolve("Foo.enso")
      val barFile = repoPath.resolve("Bar.enso")
      createStubFile(fooFile) should equal(true)
      Files.write(
        fooFile,
        "file contents".getBytes(StandardCharsets.UTF_8)
      )
      val commitResult = vcs.commit(repoPath, "New files").unsafeRunSync()
      commitResult.isRight shouldBe true

      Files.write(
        fooFile,
        "different contents".getBytes(StandardCharsets.UTF_8)
      )
      Files.write(
        barFile,
        "bar contents".getBytes(StandardCharsets.UTF_8)
      )

      barFile.toFile should exist

      val text1 = Files.readAllLines(fooFile)
      text1.get(0) should equal("different contents")

      val restoreResult = vcs.restore(repoPath, commitId = None).unsafeRunSync()
      restoreResult.isRight shouldBe true

      val text2 = Files.readAllLines(fooFile)
      text2.get(0) should equal("file contents")

      barFile.toFile should exist // TODO: verify this is the expected logic
    }

    "reset to a named saved state" in new TestCtx with InitialRepoSetup {
      val fooFile = repoPath.resolve("Foo.enso")
      createStubFile(fooFile) should equal(true)
      val text1 = "file contents"
      Files.write(
        fooFile,
        text1.getBytes(StandardCharsets.UTF_8)
      )
      val commitResult = vcs.commit(repoPath, "New files").unsafeRunSync()
      commitResult.isRight shouldBe true
      val commitId = commitResult.getOrElse(null).commitId

      val text2 = "different contents"
      Files.write(
        fooFile,
        text2.getBytes(StandardCharsets.UTF_8)
      )

      val commitResult2 = vcs.commit(repoPath, "More changes").unsafeRunSync()
      commitResult2.isRight shouldBe true

      val fileText1 = Files.readAllLines(fooFile)
      fileText1.get(0) should equal("different contents")

      val restoreResult =
        vcs.restore(repoPath, Some(commitId)).unsafeRunSync()
      restoreResult.isRight shouldBe true

      val fileText2 = Files.readAllLines(fooFile)
      fileText2.get(0) should equal("file contents")
    }

    "report problem when named save does not exist" in new TestCtx
      with InitialRepoSetup {
      val restoreResult =
        vcs.restore(repoPath, Some("invalidsha")).unsafeRunSync()
      restoreResult.isLeft shouldBe true
      restoreResult.swap.getOrElse(null) shouldBe an[SaveNotFound.type]
    }

    "report failure when requesting restore on non-existent repo" in new TestCtx {
      val listResult = vcs.restore(repoPath, None).unsafeRunSync()
      listResult.isLeft shouldBe true
      listResult.swap.getOrElse(null) shouldBe an[RepoNotFound]
    }

  }

  "VCS list" should {

    "return all commits to the repo" in new TestCtx with InitialRepoSetup {
      val files = List("Foo", "Bar", "Baz")
      files.foreach { file =>
        createStubFile(repoPath.resolve(s"$file.enso")) should equal(true)
        val commitResult1 =
          vcs.commit(repoPath, s"$file commit").unsafeRunSync()
        commitResult1.isRight shouldBe true
      }

      val listResult = vcs.list(repoPath).unsafeRunSync()
      listResult.isRight shouldBe true

      listResult.getOrElse(Nil).map(_.message) should equal(
        files.reverse.map(f => s"$f commit") ::: List("Initial commit")
      )
    }

    "return last X commits" in new TestCtx with InitialRepoSetup {
      val numOfCommits = 2
      val files        = List("Foo", "Bar", "Baz")
      files.foreach { file =>
        createStubFile(repoPath.resolve(s"$file.enso")) should equal(true)
        val commitResult1 =
          vcs.commit(repoPath, s"$file commit").unsafeRunSync()
        commitResult1.isRight shouldBe true
      }

      val listResult =
        vcs.list(repoPath, limit = Some(numOfCommits)).unsafeRunSync()
      listResult.isRight shouldBe true

      listResult.getOrElse(Nil).map(_.message) should equal(
        files.reverse.map(f => s"$f commit").take(numOfCommits)
      )
    }

    "report failure when requesting saves on non-existent repo" in new TestCtx {
      val listResult = vcs.list(repoPath).unsafeRunSync()
      listResult.isLeft shouldBe true
      listResult.swap.getOrElse(null) shouldBe an[RepoNotFound]
    }

  }

  trait TestCtx {

    val repoPath = Files.createTempDirectory(null)
    sys.addShutdownHook(FileUtils.deleteQuietly(repoPath.toFile))

    def testRepo(repoDir: Path): Repository = {
      val builder = new FileRepositoryBuilder();
      builder
        .setGitDir(repoDir.resolve(".git").toFile)
        .setMustExist(true)
        .build()
    }

    val vcs = Git()

    def listCommits(repoDir: Path): List[RevCommit] = {
      listCommits(testRepo(repoDir))
    }

    def listCommits(repo: Repository): List[RevCommit] = {
      val jgit = new JGit(repo)
      jgit.log().call().asScala.toList
    }

    def hasUntrackedFiles(repoDir: Path): Boolean = {
      hasUntrackedFiles(testRepo(repoDir))
    }

    def hasUntrackedFiles(repo: Repository): Boolean = {
      val jgit = new JGit(repo)
      jgit.status().call().isClean()
    }

    def createStubFile(path: Path): Boolean = {
      path.toFile.createNewFile()
    }

    def repoStatusIgnoreSha(r: RepoStatus) = {
      r.copy(lastCommit = r.lastCommit.copy(commitId = null))
    }
  }

  trait InitialRepoSetup { self: TestCtx =>

    setup()

    def setup(): Unit = {
      val jgit = JGit
        .init()
        .setDirectory(repoPath.toFile)
        .setBare(false)
        .call()

      jgit
        .commit()
        .setAllowEmpty(true)
        .setAll(true)
        .setMessage("Initial commit")
        .setAuthor("Enso VCS", "vcs@enso.io")
        .call()
    }
  }
}
