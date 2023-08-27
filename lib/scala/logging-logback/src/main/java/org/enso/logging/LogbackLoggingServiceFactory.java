package org.enso.logging;

import java.net.URI;

@org.openide.util.lookup.ServiceProvider(service = LoggingServiceFactory.class)
public class LogbackLoggingServiceFactory extends LoggingServiceFactory<URI> {

  @Override
  public LoggingService<URI> localServerFor(int port) {
    return new LoggingServer(port);
  }
}
