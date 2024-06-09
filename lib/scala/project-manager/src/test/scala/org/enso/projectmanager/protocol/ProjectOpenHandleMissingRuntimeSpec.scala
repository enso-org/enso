package org.enso.projectmanager.protocol

import org.enso.semver.SemVer
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite

class ProjectOpenHandleMissingRuntimeSpec
    extends ProjectOpenSpecBase
    with OverrideTestVersionSuite {

  override def testVersion: SemVer = SemVer.of(0, 0, 1)

  "project/open" should {
    behave like correctlyHandleMissingRuntimeInPresenceOfEngine()
  }
}
