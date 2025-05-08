package org.enso.logging.service.logback;

import java.util.ServiceLoader;

public abstract class OpenSearchAppender extends RemoteAppender {

  static RemoteAppender load() {
    var loader = ServiceLoader.load(RemoteAppender.class, RemoteAppender.class.getClassLoader());
    var appender =
        loader.stream()
            .filter(l -> l.type().getName().contains("OpenSearch"))
            .findFirst()
            .get()
            .get();
    return appender;
  }
}
