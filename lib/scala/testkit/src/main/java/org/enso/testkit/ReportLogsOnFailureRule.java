package org.enso.testkit;

import java.util.List;
import java.util.stream.Collectors;
import org.enso.logging.service.logback.test.utils.TestAppenderUtils;
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

  /**
   * Helper function returning currently pending log messages without materializing them in the
   * underlying appender.
   *
   * @return currently pending (formatted) log messages only
   */
  public List<String> pendingLogMessages() {
    var appender = TestAppenderUtils.getMemoryAppender();
    if (appender != null) {
      return appender.getEvents().stream()
          .map(e -> e.getFormattedMessage())
          .collect(Collectors.toList());
    } else {
      return List.of();
    }
  }

  /** Helper function to drop currently pending log messages on demand. */
  public void dropPendingMessages() {
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
