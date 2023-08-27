package org.enso.logging;

import java.net.URI;
import java.util.ServiceLoader;

public abstract class LoggingServiceFactory<T> {

  private static final ServiceLoader<LoggingServiceFactory> loader =
      ServiceLoader.load(LoggingServiceFactory.class, LoggingServiceFactory.class.getClassLoader());

  public abstract LoggingService<T> localServerFor(int port);

  @SuppressWarnings("unchecked")
  public static LoggingServiceFactory<URI> get() {
    if (_loggingServiceFactory == null) {
      _loggingServiceFactory = loader.findFirst().get();
    }
    return _loggingServiceFactory;
  }

  private static LoggingServiceFactory<URI> _loggingServiceFactory = null;
}
