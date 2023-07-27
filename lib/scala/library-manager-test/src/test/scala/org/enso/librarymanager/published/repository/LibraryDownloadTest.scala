package org.enso.librarymanager.published.repository

import org.enso.editions.Editions
import org.enso.librarymanager.published.cache.DownloadingLibraryCache
import org.enso.logger.{TestAppender, TestLogMessage}
import org.enso.pkg.PackageManager
import org.enso.testkit.WithTemporaryDirectory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory
import org.slf4j.event.Level
import ch.qos.logback.classic.Logger

import java.nio.file.Files

class LibraryDownloadTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with DownloaderTest {

  val port: Int = 47306

  "DownloadingLibraryCache" should {
    "be able to download and install libraries from a repository" in {
      val repo = new ExampleRepository

      val repoRoot = getTestDirectory.resolve("repo")
      repo.createRepository(repoRoot)
      withDownloader { cache =>
        repo.withServer(port, repoRoot) {
          cache.findCachedLibrary(
            repo.testLib.libraryName,
            repo.testLib.version
          ) shouldBe empty

          val logger = LoggerFactory
            .getLogger(classOf[DownloadingLibraryCache])
            .asInstanceOf[Logger]
          val appender = new TestAppender()
          logger.addAppender(appender)
          //logger.setContext((LoggerContext) LoggerFactory.getILoggerFactory());
          val libPath =
            cache
              .findOrInstallLibrary(
                repo.testLib.libraryName,
                repo.testLib.version,
                Editions
                  .Repository("test_repo", s"http://localhost:$port/libraries")
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
          appender.allEvents() should contain(
            TestLogMessage(
              Level.WARN,
              "License file for library [Foo.Bar:1.0.0] was missing."
            )
          )
        }
      }
    }
  }
}
