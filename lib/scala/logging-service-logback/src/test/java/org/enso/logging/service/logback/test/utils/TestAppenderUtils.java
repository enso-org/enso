package org.enso.logging.service.logback.test.utils;

import org.enso.logging.service.logback.MemoryAppender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestAppenderUtils {

  private TestAppenderUtils() {}

  private static MemoryAppender memoryAppender = null;

  public static MemoryAppender getMemoryAppender() {
    if (memoryAppender == null) {
      var ctx = LoggerFactory.getILoggerFactory();
      var rootLogger = (ch.qos.logback.classic.Logger) (ctx.getLogger(Logger.ROOT_LOGGER_NAME));
      var appender = rootLogger.getAppender(MemoryAppender.NAME);
      if (appender instanceof MemoryAppender a) {
        memoryAppender = a;
      }
    }
    return memoryAppender;
  }
}
