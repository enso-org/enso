package org.enso.launcher

import java.nio.file.{Files, Path}
import java.io.{File, IOException}

import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithTemporaryDirectory extends Suite with BeforeAndAfterEach {
  private var testDirectory: Path = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    testDirectory = Files.createTempDirectory("tmptest")
  }

  override def afterEach(): Unit = {
    super.afterEach()
    robustDeleteDirectory(testDirectory.toFile)
  }

  def getTestDirectory: Path = testDirectory.toAbsolutePath

  private def robustDeleteDirectory(dir: File): Unit = {
    def tryRemoving(retry: Int): Unit = {
      try {
        FileUtils.deleteDirectory(dir)
      } catch {
        case e: IOException =>
          if (retry > 0) {
            Thread.sleep(100)
            tryRemoving(retry - 1)
          } else {
            throw new RuntimeException("Cannot remove temporary test files", e)
          }
      }
    }

    tryRemoving(30)
  }
}
