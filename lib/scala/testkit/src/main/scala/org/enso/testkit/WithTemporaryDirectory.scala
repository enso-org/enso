package org.enso.testkit

import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterEach, Suite}
import java.io.{File, IOException}
import java.nio.file.{Files, Path}

/** Creates a separate temporary directory for each test.
  */
trait WithTemporaryDirectory
    extends Suite
    with BeforeAndAfterEach
    with HasTestDirectory {
  private var testDirectory: Path = _

  /** @inheritdoc
    */
  override def beforeEach(): Unit = {
    prepareTemporaryDirectory()
    super.beforeEach()
  }

  /** @inheritdoc
    */
  override def afterEach(): Unit = {
    super.afterEach()
    robustDeleteDirectory(testDirectory.toFile)
  }

  /** Returns the temporary directory for this test. */
  def getTestDirectory: Path = testDirectory.toAbsolutePath.normalize

  /** Tries to remove the directory, retrying every 200ms for 3 seconds. If
    * unsuccessful, schedules the directory for deletion on JVM shutdown.
    *
    * This is used because there may be some lag between the test finalizing and
    * the filesystem allowing to remove the files (especially on Windows,
    * because if the test runs other executables, they may take a moment to
    * terminate even after the test completed).
    */
  def robustDeleteDirectory(dir: File): Unit = {
    @scala.annotation.tailrec
    def tryRemoving(retry: Int): Unit = {
      try {
        FileUtils.deleteDirectory(dir)
      } catch {
        case _: IOException =>
          if (retry > 0) {
            Thread.sleep(200)
            tryRemoving(retry - 1)
          } else {
            FileUtils.forceDeleteOnExit(dir)
          }
      }
    }

    tryRemoving(15)
  }

  private def prepareTemporaryDirectory(): Unit = {
    testDirectory = Files.createTempDirectory("enso-test")
  }
}
