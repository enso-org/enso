package org.enso.editions.updater

import nl.gn0s1s.bump.SemVer
import org.enso.librarymanager.published.repository.ExampleRepository
import org.enso.testkit.WithTemporaryDirectory
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class UpdatingEditionProviderTest
    extends AnyWordSpec
    with Matchers
    with Inside
    with WithTemporaryDirectory {

  val repo      = new ExampleRepository
  def port: Int = 47309
  def repoPath  = getTestDirectory.resolve("repo")
  def cachePath = getTestDirectory.resolve("cache")

  override def beforeEach(): Unit = {
    super.beforeEach()
    repo.createRepository(repoPath)
    Files.createDirectories(cachePath)
  }

  def makeEditionProvider(): UpdatingEditionProvider =
    new UpdatingEditionProvider(
      List(cachePath),
      cachePath,
      Seq(s"http://localhost:$port/editions")
    )

  "UpdatingEditionProvider" should {
    "list installed editions and download new ones if asked" in {
      repo.withServer(port, repoPath) {
        val provider = makeEditionProvider()
        provider.findAvailableEditions(update = false) shouldBe empty
        provider.findAvailableEditions(update =
          true
        ) should contain theSameElementsAs Seq("testlocal")
      }
    }

    "try updating editions if an edition is missing" in {
      repo.withServer(port, repoPath) {
        val provider = makeEditionProvider()
        provider.findAvailableEditions(update = false) shouldBe empty
        inside(provider.findEditionForName("testlocal")) {
          case Right(edition) =>
            edition.engineVersion shouldEqual Some(SemVer(0, 0, 0))
        }
      }
    }
  }
}
