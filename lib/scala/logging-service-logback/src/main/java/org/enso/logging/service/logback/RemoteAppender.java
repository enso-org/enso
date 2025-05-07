package org.enso.logging.service.logback;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import java.net.URI;
import java.util.concurrent.ThreadPoolExecutor;

public abstract class RemoteAppender extends AppenderBase<ILoggingEvent> {

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
}
