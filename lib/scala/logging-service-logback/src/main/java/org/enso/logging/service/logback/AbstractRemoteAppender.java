package org.enso.logging.service.logback;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import java.net.URI;
import java.util.ServiceLoader;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * Base class for any appender sending logs to a remote endpoint. Must specify; - an endpoint where
 * logs should be forwarded to - a thread executor that should be used to processes collected logs
 */
public abstract class AbstractRemoteAppender extends AppenderBase<ILoggingEvent> {

  protected ThreadPoolExecutor backgroundThreadService;
  protected URI endpoint;
  protected boolean logConnectionFailures;

  public void setLogConnectionFailures(boolean logConnectionFailures) {
    this.logConnectionFailures = logConnectionFailures;
  }

  public void setExecutor(ThreadPoolExecutor executor) {
    this.backgroundThreadService = executor;
  }

  public void setEndpoint(URI endpoint) {
    this.endpoint = endpoint;
  }

  @Override
  public void start() {
    assert backgroundThreadService != null;
    assert endpoint != null;
    super.start();
  }

  public abstract boolean canLogTelemetry();

  public abstract boolean canLogGenericMessages();

  static AbstractRemoteAppender loadGenericRemoteAppender() {
    var loader =
        ServiceLoader.load(
            AbstractRemoteAppender.class, AbstractRemoteAppender.class.getClassLoader());
    return loader.stream()
        .filter(l -> l.get().canLogGenericMessages())
        .findFirst()
        .map(ServiceLoader.Provider::get)
        .orElse(null);
  }

  static AbstractRemoteAppender loadTelemetryAppender() {
    var loader =
        ServiceLoader.load(
            AbstractRemoteAppender.class, AbstractRemoteAppender.class.getClassLoader());
    return loader.stream()
        .filter(l -> l.get().canLogTelemetry())
        .findFirst()
        .map(ServiceLoader.Provider::get)
        .orElse(null);
  }
}
