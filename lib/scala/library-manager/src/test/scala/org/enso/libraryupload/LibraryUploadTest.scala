package org.enso.libraryupload

import org.enso.librarymanager.published.repository.EmptyRepository
import org.enso.libraryupload.auth.SimpleHeaderToken
import org.enso.pkg.PackageManager
import org.enso.testkit.WithTemporaryDirectory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LibraryUploadTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory {

  def port: Int = 47305

  "LibraryUploader" should {
    "do stuff (TODO)" in {
      val projectRoot = getTestDirectory.resolve("lib_root")
      val repoRoot    = getTestDirectory.resolve("repo")

      PackageManager.Default.create(
        projectRoot.toFile,
        name      = "UploadProj",
        namespace = "Tests",
        version   = "1.2.3"
      )

      val server = EmptyRepository.startServer(port, repoRoot)
      try {
        val uploadUrl = s"http://localhost:$port/upload"
        val token     = SimpleHeaderToken("Auth-Token", "TODO")
        import scala.concurrent.ExecutionContext.Implicits.global
        LibraryUploader.uploadLibrary(projectRoot, uploadUrl, token).get
      } finally {
        server.kill(killDescendants    = true)
        server.join(waitForDescendants = true)
      }
    }
  }
}
