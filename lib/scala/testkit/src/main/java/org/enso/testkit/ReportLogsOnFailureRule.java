package org.enso.testkit;

import org.enso.logging.service.logback.test.TestAppenderUtils;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

public final class ReportLogsOnFailureRule extends TestWatcher {

  @Override
  protected void succeeded(Description description) {
    var appender = TestAppenderUtils.getMemoryAppender();
    if (appender != null) {
      appender.reset();
    }
  }

  @Override
  protected void failed(Throwable e, Description description) {
    var appender = TestAppenderUtils.getMemoryAppender();
    appender.flush();
    appender.reset();
  }
}
