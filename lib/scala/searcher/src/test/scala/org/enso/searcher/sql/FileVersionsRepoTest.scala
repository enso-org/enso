package org.enso.searcher.sql

import java.io.File
import java.nio.file.{Files, Path}
import java.util

import org.enso.testkit.RetrySpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class FileVersionsRepoTest extends AnyWordSpec with Matchers with RetrySpec {

  val Timeout: FiniteDuration = 20.seconds

  val tmpdir: Path = {
    val tmp = Files.createTempDirectory("versions-repo-test")
    sys.addShutdownHook {
      Files.list(tmp).forEach { path =>
        path.toFile.delete()
      }
      tmp.toFile.delete()
    }
    tmp
  }

  def withRepo(test: SqlVersionsRepo => Any): Any = {
    val tmpdb = Files.createTempFile(tmpdir, "versions-repo", ".db")
    val repo  = SqlVersionsRepo(tmpdb.toFile)
    Await.ready(repo.init, Timeout)
    try test(repo)
    finally {
      Await.ready(repo.clean, Timeout)
      repo.close()
    }
  }

  def nextDigest(): Array[Byte] =
    Random.nextBytes(28)

  "FileVersionsRepo" should {

    "init idempotent" in withRepo { repo =>
      Await.result(repo.init, Timeout)
    }

    "insert digest" taggedAs Retry in withRepo { repo =>
      val file   = new File("/foo/bar")
      val digest = nextDigest()
      val action =
        for {
          v1 <- repo.setVersion(file, digest)
          v2 <- repo.getVersion(file)
        } yield (v1, v2)

      val (v1, v2) = Await.result(action, Timeout)
      v1 shouldBe None
      v2 shouldBe a[Some[_]]
      util.Arrays.equals(v2.get, digest) shouldBe true
    }

    "set digest" taggedAs Retry in withRepo { repo =>
      val file    = new File("/foo/bar")
      val digest1 = nextDigest()
      val digest2 = nextDigest()
      val action =
        for {
          v1 <- repo.setVersion(file, digest1)
          v2 <- repo.setVersion(file, digest2)
          v3 <- repo.getVersion(file)
        } yield (v1, v2, v3)

      val (v1, v2, v3) = Await.result(action, Timeout)
      v1 shouldBe None
      v2 shouldBe a[Some[_]]
      v3 shouldBe a[Some[_]]
      util.Arrays.equals(v2.get, digest1) shouldBe true
      util.Arrays.equals(v3.get, digest2) shouldBe true
    }

    "update digest" taggedAs Retry in withRepo { repo =>
      val file    = new File("/foo/bazz")
      val digest1 = nextDigest()
      val digest2 = nextDigest()
      val digest3 = nextDigest()
      val action =
        for {
          b1 <- repo.updateVersion(file, digest1)
          v2 <- repo.setVersion(file, digest2)
          b2 <- repo.updateVersion(file, digest2)
          b3 <- repo.updateVersion(file, digest3)
          b4 <- repo.updateVersion(file, digest3)
          v3 <- repo.getVersion(file)
        } yield (v2, v3, b1, b2, b3, b4)

      val (v2, v3, b1, b2, b3, b4) = Await.result(action, Timeout)
      v2 shouldBe a[Some[_]]
      v3 shouldBe a[Some[_]]
      util.Arrays.equals(v2.get, digest1) shouldBe true
      util.Arrays.equals(v3.get, digest3) shouldBe true
      b1 shouldBe true
      b2 shouldBe false
      b3 shouldBe true
      b4 shouldBe false
    }

    "delete digest" taggedAs Retry in withRepo { repo =>
      val file   = new File("/foo/bar")
      val digest = nextDigest()
      val action =
        for {
          v1 <- repo.setVersion(file, digest)
          _  <- repo.remove(file)
          v2 <- repo.getVersion(file)
        } yield (v1, v2)

      val (v1, v2) = Await.result(action, Timeout)
      v1 shouldEqual None
      v2 shouldEqual None
    }
  }
}
