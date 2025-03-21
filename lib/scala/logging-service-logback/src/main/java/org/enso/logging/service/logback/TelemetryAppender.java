package org.enso.logging.service.logback;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import java.net.URI;
import java.util.ServiceLoader;
import java.util.concurrent.ThreadPoolExecutor;

public abstract class TelemetryAppender extends AppenderBase<ILoggingEvent> {

  static TelemetryAppender load() {
    var loader =
        ServiceLoader.load(TelemetryAppender.class, TelemetryAppender.class.getClassLoader());
    var appender = loader.findFirst().get();
    return appender;
  }

  protected ThreadPoolExecutor backgroundThreadService;
  protected URI endpoint;

  void setExecutor(ThreadPoolExecutor executor) {
    this.backgroundThreadService = executor;
  }

  void setEndpoint(URI endpoint) {
    this.endpoint = endpoint;
  }

  @Override
  public void start() {
    assert backgroundThreadService != null;
    assert endpoint != null;
    super.start();
  }
}
