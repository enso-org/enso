package org.enso.runtimeversionmanager.test

import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.CurrentVersion
import org.scalatest.{BeforeAndAfterAll, Suite}

/** The suite that overrides the [[CurrentVersion]]. */
trait OverrideTestVersionSuite extends Suite with BeforeAndAfterAll {

  private val originalVersion: SemVer = CurrentVersion.version

  /** The version that will be used in this suite. */
  def testVersion: SemVer

  override def beforeAll(): Unit = {
    CurrentVersion.internalOverrideVersion(testVersion)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    CurrentVersion.internalOverrideVersion(originalVersion)
  }

}
