package org.enso.librarymanager.published.repository

import org.enso.distribution.FileSystem
import org.enso.editions.Editions
import org.enso.librarymanager.published.cache.DownloadingLibraryCache
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LibraryDownloadTest extends AnyWordSpec with Matchers {

  val port: Int = 54325

  "DownloadingLibraryCache" should {
    "be able to download and install libraries from a repository" in {
      val repo = new ExampleRepository
      FileSystem.withTemporaryDirectory("enso-test-repo") { repoRoot =>
        repo.createRepository(repoRoot)
        val server = repo.runServer(port, repoRoot)

        // TODO
        val cache = new DownloadingLibraryCache(???, ???, ???, ???, ???)

        cache.findCachedLibrary(
          repo.testLib.libraryName,
          repo.testLib.version
        ) shouldBe empty

        cache
          .findOrInstallLibrary(
            repo.testLib.libraryName,
            repo.testLib.version,
            Editions
              .Repository("test_repo", s"http://localhost:$port/libraries")
          )
          .get

        server.destroy()
        server.waitFor()
      }
    }
  }
}
