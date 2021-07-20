package org.enso.librarymanager.published.repository

import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.distribution.locking.{
  LockUserInterface,
  Resource,
  ResourceManager,
  ThreadSafeFileLockManager
}
import org.enso.distribution.{FileSystem, TemporaryDirectoryManager}
import org.enso.editions.Editions
import org.enso.librarymanager.published.cache.DownloadingLibraryCache
import org.enso.pkg.PackageManager
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LibraryDownloadTest extends AnyWordSpec with Matchers {

  val port: Int = 54325

  "DownloadingLibraryCache" should {
    "be able to download and install libraries from a repository" in {
      val repo = new ExampleRepository
      FileSystem.withTemporaryDirectory("enso-test-lib") { tmp =>
        val repoRoot = tmp.resolve("repo")
        repo.createRepository(repoRoot)
        val lockManager     = new ThreadSafeFileLockManager(tmp.resolve("locks"))
        val resourceManager = new ResourceManager(lockManager)
        val cache = new DownloadingLibraryCache(
          cacheRoot = tmp.resolve("cache"),
          temporaryDirectoryManager =
            new TemporaryDirectoryManager(tmp.resolve("tmp"), resourceManager),
          resourceManager = resourceManager,
          lockUserInterface = new LockUserInterface {
            override def startWaitingForResource(resource: Resource): Unit =
              println(s"Waiting for ${resource.name}")

            override def finishWaitingForResource(resource: Resource): Unit =
              println(s"${resource.name} is ready")
          },
          progressReporter = new ProgressReporter {
            override def trackProgress(
              message: String,
              task: TaskProgress[_]
            ): Unit = {}
          }
        )

        val server = repo.startServer(port, repoRoot)
        try {
          cache.findCachedLibrary(
            repo.testLib.libraryName,
            repo.testLib.version
          ) shouldBe empty

          val libPath = cache
            .findOrInstallLibrary(
              repo.testLib.libraryName,
              repo.testLib.version,
              Editions
                .Repository("test_repo", s"http://localhost:$port/libraries")
            )
            .get
          val pkg = PackageManager.Default.loadPackage(libPath.toFile).get
          pkg.name shouldEqual "Bar"
          val sources = pkg.listSources
          sources should have size 1
          sources.head.file.getName shouldEqual "Main.enso"
          // TODO [RW] check that the license is missing
        } finally {
          server.destroy()
          server.waitFor()
        }
      }
    }
  }
}
