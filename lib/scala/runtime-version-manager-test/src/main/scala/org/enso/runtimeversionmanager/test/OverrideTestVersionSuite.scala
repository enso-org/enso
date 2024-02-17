package org.enso.runtimeversionmanager.test

import com.github.zafarkhaja.semver.Version
import org.enso.runtimeversionmanager.CurrentVersion
import org.scalatest.{BeforeAndAfterAll, Suite}

/** The suite that overrides the [[CurrentVersion]]. */
trait OverrideTestVersionSuite extends Suite with BeforeAndAfterAll {

  private val originalVersion: Version = CurrentVersion.version

  /** The version that will be used in this suite. */
  def testVersion: Version

  override def beforeAll(): Unit = {
    CurrentVersion.internalOverrideVersion(testVersion)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    CurrentVersion.internalOverrideVersion(originalVersion)
  }

}
