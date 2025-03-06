package org.enso.librarymanager.published.repository

import org.enso.editions.Editions
import org.enso.librarymanager.published.cache.DownloadingLibraryCache
import org.enso.librarymanager.test.published.repository.{
  DownloaderTest,
  ExampleRepository
}
import org.enso.pkg.PackageManager
import org.enso.testkit.{RetrySpec, WithTemporaryDirectory}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.event.Level
import org.enso.logger.ObservedMessage

import java.nio.file.{Files, Path}
import org.slf4j.LoggerFactory

class LibraryDownloadTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with DownloaderTest
    with RetrySpec {

  val port: Int = 47306

  "DownloadingLibraryCache" should {
    "be able to download and install libraries from a repository" taggedAs Retry in {
      val repo = new ExampleRepository(Path.of("../../../"))

      val repoRoot = getTestDirectory.resolve("repo")
      repo.createRepository(repoRoot)
      withDownloader { cache =>
        repo.withServer(port, repoRoot) {
          cache.findCachedLibrary(
            repo.testLib.libraryName,
            repo.testLib.version
          ) shouldBe empty
          val logger = LoggerFactory.getLogger(classOf[DownloadingLibraryCache])

          val allLogs = ObservedMessage.collect(
            logger,
            () => {
              val libPath =
                cache
                  .findOrInstallLibrary(
                    repo.testLib.libraryName,
                    repo.testLib.version,
                    Editions
                      .Repository(
                        "test_repo",
                        s"http://localhost:$port/libraries"
                      )
                  )
                  .get
              val pkg =
                PackageManager.Default.loadPackage(libPath.location.toFile).get
              pkg.normalizedName shouldEqual "Bar"
              val sources = pkg.listSources()
              sources should have size 1
              sources.head.file.getName shouldEqual "Main.enso"
              assert(
                Files.notExists(libPath / "LICENSE.md"),
                "The license file should not exist as it was not provided " +
                "in the repository."
              )
            }
          )
          val found = allLogs
            .stream()
            .filter(m => {
              val expMsg =
                "License file for library [Foo.Bar:1.0.0] was missing."
              m.getLevel() == Level.WARN && m.getFormattedMessage() == expMsg
            })
            .findAny
          found.isPresent() shouldBe true
        }
      }
    }
  }
}
