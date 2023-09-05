package org.enso.logging;

import java.net.URI;
import java.util.ServiceLoader;

public abstract class LoggingServiceFactory<T> {

  private static volatile LoggingServiceFactory _loggingServiceFactory;
  private static Object _lock = new Object();

  public abstract LoggingService<T> localServerFor(int port);

  @SuppressWarnings("unchecked")
  public static LoggingServiceFactory<URI> get() {
    LoggingServiceFactory<URI> result = _loggingServiceFactory;
    if (result == null) {
      synchronized (_lock) {
        result = _loggingServiceFactory;
        if (result == null) {
          // Can't initialize in static initializer because Config has to be able to read runtime
          // env vars
          ServiceLoader<LoggingServiceFactory> loader =
              ServiceLoader.load(
                  LoggingServiceFactory.class, LoggingServiceFactory.class.getClassLoader());
          result = loader.findFirst().get();
          _loggingServiceFactory = result;
        }
      }
    }
    return result;
  }
}
