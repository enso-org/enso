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

  "VCS" should {
    "correctly initialize the repository" in new TestCtx {

      val targetRepo = testDirPath.resolve(".git")
      targetRepo.toFile shouldNot exist
      val path   = testDirPath
      val result = vcs.init(path).unsafeRunSync()
      result shouldBe Right(())
      targetRepo.toFile should exist

    }

    "fail to create a repo for non-existent project" in new TestCtx {
      val path   = testDirPath.resolve("foo")
      val result = vcs.init(path).unsafeRunSync()
      result.isLeft shouldBe true
      result.swap.getOrElse(null) shouldBe an[ProjectNotFound]

      val targetRepo = path.resolve(".git")
      targetRepo.toFile shouldNot exist
    }

    "be able to commit to the repo" in new TestCtx {
      val path       = testDirPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      val commitResult1 = vcs.commit(path, "First").unsafeRunSync()
      commitResult1.isRight shouldBe true

      val commitResult2 = vcs.commit(path, "Second").unsafeRunSync()
      commitResult2.isRight shouldBe true

      val revisions = listCommits(path)
      revisions should have length 3
      revisions(0).getFullMessage() should equal("Second")
      revisions(1).getFullMessage() should equal("First")
      revisions(2).getFullMessage() should equal("Initial commit")
      path.toFile should exist
    }

    "be able to list all named saves" in new TestCtx {
      val path       = testDirPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      val commitResult1 = vcs.commit(path, "First").unsafeRunSync()
      commitResult1.isRight shouldBe true

      val commitResult2 = vcs.commit(path, "Second").unsafeRunSync()
      commitResult2.isRight shouldBe true

      val savedResult = vcs.list(path).unsafeRunSync()
      savedResult.isRight shouldBe true

      val saved = savedResult.getOrElse(Nil)
      saved should have length 3
      saved(0) should equal("Second")
      saved(1) should equal("First")
      saved(2) should equal("Initial commit")
    }

    "report dirty status when untracked files are added" in new TestCtx {
      val path       = testDirPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      val modifiedResult1 = vcs.modified(path).unsafeRunSync()
      modifiedResult1.isRight shouldBe true
      modifiedResult1.getOrElse(null) should equal(false)

      createStubFile(path.resolve("Foo.enso")) should equal(true)

      val modifiedResult2 = vcs.modified(path).unsafeRunSync()
      modifiedResult2.isRight shouldBe true
      modifiedResult2.getOrElse(null) shouldBe true
    }

    "not report dirty status when untracked files were committed" in new TestCtx {
      val path       = testDirPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      val modifiedResult1 = vcs.modified(path).unsafeRunSync()
      modifiedResult1.isRight shouldBe true
      modifiedResult1.getOrElse(null) should equal(false)

      createStubFile(path.resolve("Foo.enso")) should equal(true)
      createStubFile(path.resolve("Bar.enso")) should equal(true)
      val commitResult = vcs.commit(path, "New files").unsafeRunSync()
      commitResult.isRight shouldBe true

      val modifiedResult2 = vcs.modified(path).unsafeRunSync()
      modifiedResult2.isRight shouldBe true
      modifiedResult2.getOrElse(null) shouldBe false
    }

    "report dirty status when tracked files were modified" in new TestCtx {
      val path       = testDirPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      val fooFile = path.resolve("Foo.enso")
      createStubFile(fooFile) should equal(true)
      val commitResult = vcs.commit(path, "New files").unsafeRunSync()
      commitResult.isRight shouldBe true

      Files.write(
        fooFile,
        "file contents".getBytes(StandardCharsets.UTF_8)
      )

      val modifiedResult2 = vcs.modified(path).unsafeRunSync()
      modifiedResult2.isRight shouldBe true
      modifiedResult2.getOrElse(null) shouldBe true
    }

    "reset to clean state" in new TestCtx {
      val path       = testDirPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      val fooFile = path.resolve("Foo.enso")
      createStubFile(fooFile) should equal(true)
      Files.write(
        fooFile,
        "file contents".getBytes(StandardCharsets.UTF_8)
      )
      val commitResult = vcs.commit(path, "New files").unsafeRunSync()
      commitResult.isRight shouldBe true

      Files.write(
        fooFile,
        "different contents".getBytes(StandardCharsets.UTF_8)
      )

      val text1 = Files.readAllLines(fooFile)
      text1.get(0) should equal("different contents")

      val restoreResult = vcs.restore(path).unsafeRunSync()
      restoreResult.isRight shouldBe true

      val text2 = Files.readAllLines(fooFile)
      text2.get(0) should equal("file contents")
    }

    "list all commits" in new TestCtx {
      val path       = testDirPath
      val initResult = vcs.init(path).unsafeRunSync()
      initResult shouldBe Right(())

      val files = List("Foo", "Bar", "Baz")
      files.foreach { file =>
        createStubFile(path.resolve(s"$file.enso")) should equal(true)
        val commitResult1 = vcs.commit(path, s"$file commit").unsafeRunSync()
        commitResult1.isRight shouldBe true
      }

      val listResult = vcs.list(path).unsafeRunSync()
      listResult.isRight shouldBe true

      listResult.getOrElse(Nil) should equal(
        files.reverse.map(f => s"$f commit") ::: List("Initial commit")
      )
    }

  }

  trait TestCtx {

    val testDirPath = Files.createTempDirectory(null)
    sys.addShutdownHook(FileUtils.deleteQuietly(testDirPath.toFile))

    def testRepo(repoDir: Path): Repository = {
      val builder = new FileRepositoryBuilder();
      builder
        .setGitDir(repoDir.resolve(Git.DefaultGitRepoDir).toFile)
        .setMustExist(true)
        .build()
    }

    val vcs = new Git

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
  }
}
