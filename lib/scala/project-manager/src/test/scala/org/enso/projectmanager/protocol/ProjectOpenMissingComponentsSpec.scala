package org.enso.projectmanager.protocol

class ProjectOpenMissingComponentsSpec extends ProjectOpenSpecBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    uninstallEngine(defaultVersion)
  }

  "project/open" should {
    behave like correctlyHandleMissingComponents()
  }
}
