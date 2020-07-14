package org.enso.searcher.sql

import java.io.File
import java.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class FileVersionsRepoTest
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfter
    with BeforeAndAfterAll {

  val Timeout: FiniteDuration = 10.seconds

  val repo = SqlVersionsRepo()

  override def beforeAll(): Unit = {
    Await.ready(repo.init, Timeout)
  }

  override def afterAll(): Unit = {
    repo.close()
  }

  def nextDigest(): Array[Byte] =
    Random.nextBytes(28)

  before {
    Await.ready(repo.clean, Timeout)
  }

  "FileVersionsRepo" should {

    "insert digest" in {
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

    "update digest" in {
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

    "delete digest" in {
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
