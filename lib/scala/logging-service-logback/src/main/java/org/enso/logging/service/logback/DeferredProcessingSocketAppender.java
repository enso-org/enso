package org.enso.logging.service.logback;

import ch.qos.logback.classic.net.SocketAppender;
import ch.qos.logback.classic.spi.ILoggingEvent;

/** SocketAppender that ensures that MDC info is not lost during serialization. */
public class DeferredProcessingSocketAppender extends SocketAppender {
  @Override
  protected void append(ILoggingEvent event) {
    event.prepareForDeferredProcessing();
    super.append(event);
  }
}
