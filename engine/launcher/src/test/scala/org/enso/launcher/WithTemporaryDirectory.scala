package org.enso.launcher

import java.nio.file.{Files, Path}

import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithTemporaryDirectory extends Suite with BeforeAndAfterEach {
  private var testDirectory: Path = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    testDirectory = Files.createTempDirectory(Path.of("."), "tmptest")
  }

  override def afterEach(): Unit = {
    super.afterEach()
    FileUtils.deleteDirectory(testDirectory.toFile)
  }

  def getTestDirectory: Path = testDirectory
}
