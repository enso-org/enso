package org.enso.logging;

public class LoggingServiceAlreadySetup extends RuntimeException {
  public LoggingServiceAlreadySetup() {
    super("Logging Service already setup");
  }
}
