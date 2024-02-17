package org.enso.projectmanager.protocol
import com.github.zafarkhaja.semver.Version
import org.enso.logger.ReportLogsOnFailure
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite

class ProjectOpenMissingComponentsSpec
    extends ProjectOpenSpecBase
    with OverrideTestVersionSuite
    with ReportLogsOnFailure {

  override val testVersion: Version = defaultVersion

  override def beforeAll(): Unit = {
    super.beforeAll()
    uninstallEngine(defaultVersion)
  }

  "project/open" should {
    behave like correctlyHandleMissingComponents()
  }

}
