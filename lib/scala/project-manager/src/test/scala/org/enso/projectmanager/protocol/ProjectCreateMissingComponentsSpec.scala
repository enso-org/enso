package org.enso.projectmanager.protocol

class ProjectCreateMissingComponentsSpec extends ProjectCreateSpecBase {
  "project/create" should {
    behave like correctlyHandleMissingComponents()
  }
}
