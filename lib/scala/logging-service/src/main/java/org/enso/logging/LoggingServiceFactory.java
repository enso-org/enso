package org.enso.logging;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.net.URI;

public abstract class LoggingServiceFactory<T> {

  public abstract LoggingService<T> localServerFor(int port);

  @SuppressWarnings("unchecked")
  public static LoggingServiceFactory<URI> get() {
    if (_loggingServiceFactory == null) {
      Config c = ConfigFactory.load("enso-logging.conf");
      if (c.hasPath(implClassKey)) {
        try {
          String clazzName = c.getString(implClassKey);
          Class<?> clazz = Class.forName(clazzName);
          _loggingServiceFactory =
              (LoggingServiceFactory<URI>) clazz.getDeclaredConstructor().newInstance();
        } catch (Throwable e) {
          e.printStackTrace();
          System.err.println("Failed to initialize LoggingServiceFactory configuration class");
          return null;
        }

      } else {
        System.err.println("Missing log configuration class key: " + implClassKey);
        return null;
      }
    }
    return _loggingServiceFactory;
  }

  private static LoggingServiceFactory<URI> _loggingServiceFactory = null;

  private static final String implClassKey = LoggingServiceFactory.class.getName() + ".impl.class";
}
