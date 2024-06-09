package org.enso.projectmanager.protocol

import org.enso.logger.ReportLogsOnFailure

class ProjectCreateMissingComponentsSpec
    extends ProjectCreateSpecBase
    with ReportLogsOnFailure {
  "project/create" should {
    behave like correctlyHandleMissingComponents()
  }
}
