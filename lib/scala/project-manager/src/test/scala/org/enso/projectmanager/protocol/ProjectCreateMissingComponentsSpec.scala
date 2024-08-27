package org.enso.projectmanager.protocol

import org.enso.logging.service.logback.test.provider.ReportLogsOnFailure

class ProjectCreateMissingComponentsSpec
    extends ProjectCreateSpecBase
    with ReportLogsOnFailure {
  "project/create" should {
    behave like correctlyHandleMissingComponents()
  }
}
