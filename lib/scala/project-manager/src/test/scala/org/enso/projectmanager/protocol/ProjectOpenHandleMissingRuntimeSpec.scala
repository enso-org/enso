package org.enso.projectmanager.protocol

class ProjectOpenHandleMissingRuntimeSpec extends ProjectOpenSpecBase {

  "project/open" should {
    behave like correctlyHandleMissingRuntimeInPresenceOfEngine()
  }
}
