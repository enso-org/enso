package org.enso.projectmanager.protocol

import com.github.zafarkhaja.semver.Version
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite

class ProjectOpenHandleMissingRuntimeSpec
    extends ProjectOpenSpecBase
    with OverrideTestVersionSuite {

  override def testVersion: Version = Version.of(0, 0, 1)

  "project/open" should {
    behave like correctlyHandleMissingRuntimeInPresenceOfEngine()
  }
}
