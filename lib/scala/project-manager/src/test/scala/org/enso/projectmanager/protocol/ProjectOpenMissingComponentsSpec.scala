package org.enso.projectmanager.protocol
import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite

class ProjectOpenMissingComponentsSpec
    extends ProjectOpenSpecBase
    with OverrideTestVersionSuite {

  override val testVersion: SemVer = defaultVersion

  override def beforeAll(): Unit = {
    super.beforeAll()
    uninstallEngine(defaultVersion)
  }

  "project/open" should {
    behave like correctlyHandleMissingComponents()
  }

}
