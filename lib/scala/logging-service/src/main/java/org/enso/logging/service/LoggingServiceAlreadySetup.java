package org.enso.logging.service;

public class LoggingServiceAlreadySetup extends RuntimeException {
  public LoggingServiceAlreadySetup() {
    super("Logging Service already setup");
  }
}
