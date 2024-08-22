package org.enso.logging;

import java.net.URI;
import org.enso.logging.service.LoggingService;
import org.enso.logging.service.LoggingServiceFactory;

@org.openide.util.lookup.ServiceProvider(service = LoggingServiceFactory.class)
public class LogbackLoggingServiceFactory extends LoggingServiceFactory<URI> {

  @Override
  public LoggingService<URI> localServerFor(int port) {
    return new LoggingServer(port);
  }
}
