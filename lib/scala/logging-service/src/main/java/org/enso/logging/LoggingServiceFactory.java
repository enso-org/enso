package org.enso.logging;

import java.net.URI;
import java.util.ServiceLoader;

public abstract class LoggingServiceFactory<T> {

  private static LoggingServiceFactory _loggingServiceFactory;

  static {
    ServiceLoader<LoggingServiceFactory> loader =
        ServiceLoader.load(
            LoggingServiceFactory.class, LoggingServiceFactory.class.getClassLoader());
    _loggingServiceFactory = loader.findFirst().get();
  }

  public abstract LoggingService<T> localServerFor(int port);

  @SuppressWarnings("unchecked")
  public static LoggingServiceFactory<URI> get() {
    return _loggingServiceFactory;
  }
}
