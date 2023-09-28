package org.enso.projectmanager.protocol
import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite

class ProjectOpenHandleMissingRuntimeSpec
    extends ProjectOpenSpecBase
    with OverrideTestVersionSuite {

  override def testVersion: SemVer = SemVer(0, 0, 1)

  "project/open" should {
    behave like correctlyHandleMissingRuntimeInPresenceOfEngine()
  }
}
