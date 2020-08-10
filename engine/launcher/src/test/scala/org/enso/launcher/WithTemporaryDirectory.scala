package org.enso.launcher

import java.nio.file.{Files, Path}
import java.io.{File, IOException}

import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterEach, Suite}

/**
  * Creates a separate temporary directory for each test.
  */
trait WithTemporaryDirectory extends Suite with BeforeAndAfterEach {
  private var testDirectory: Path = _

  /**
    * @inheritdoc
    */
  override def beforeEach(): Unit = {
    super.beforeEach()
    testDirectory = Files.createTempDirectory("enso-test")
  }

  /**
    * @inheritdoc
    */
  override def afterEach(): Unit = {
    super.afterEach()
    robustDeleteDirectory(testDirectory.toFile)
  }

  /**
    * Returns the temporary directory for this test.
    */
  def getTestDirectory: Path = testDirectory.toAbsolutePath

  /**
    * Tries to remove the directory, retrying every 100ms for 3 seconds.
    *
    * This is used because there may be some lag between the test finalizing and
    * the filesystem allowing to remove the files (especially on Windows,
    * because if the test runs other executables, they may take a moment to
    * terminate even after the test completed).
    */
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
