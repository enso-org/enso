package org.enso.example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggingTestUtils {
  private static Logger logger = LoggerFactory.getLogger(LoggingTestUtils.class);

  public static boolean logSomething() {
    logger.info("Logging something");
    return true;
  }
}
