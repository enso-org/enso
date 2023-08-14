package org.enso.logging;

import org.enso.logger.LoggerContextSetup;
import org.slf4j.event.Level;

public class Local extends LoggingService {

  private String componentName;

  public Local(String componentName) {
    this.componentName = componentName;
  }

  @Override
  public void teardown() {
    LoggerContextSetup.teardown();
  }

  public void start(Level logLevel) {
    var logConfig = this.getClass().getResourceAsStream("/" + componentName + ".logback.xml");
    LoggerContextSetup.setup(logLevel, componentName, logConfig, "console", null, 0);
  }
}
