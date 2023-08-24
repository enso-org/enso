package org.enso.logging;

import java.net.URI;

public class LogbackLoggingServiceFactory extends LoggingServiceFactory<URI> {

  @Override
  public LoggingService<URI> localServerFor(int port) {
    return new LoggingServer(port);
  }
}
