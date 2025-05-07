package org.enso.logging.service.logback;

import java.util.ServiceLoader;

public abstract class TelemetryAppender extends RemoteAppender {

  static RemoteAppender load() {
    var loader = ServiceLoader.load(RemoteAppender.class, RemoteAppender.class.getClassLoader());
    var appender =
        loader.stream()
            .filter(l -> l.type().getName().contains("Telemetry"))
            .findFirst()
            .get()
            .get();
    return appender;
  }
}
