package org.enso.logging.service.logback;

import java.net.URI;
import org.enso.logging.service.LoggingService;
import org.enso.logging.service.LoggingServiceFactory;

public class LogbackLoggingServiceFactory extends LoggingServiceFactory<URI> {

  @Override
  public LoggingService<URI> localServerFor(int port) {
    return new LoggingServer(port);
  }
}
