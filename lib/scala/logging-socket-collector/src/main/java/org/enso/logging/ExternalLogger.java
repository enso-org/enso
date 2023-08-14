package org.enso.logging;

import java.net.ConnectException;
import java.net.URI;
import org.slf4j.event.Level;

public class ExternalLogger extends LoggingService {
  private URI targetURI;

  public ExternalLogger(URI uri) {
    this.targetURI = uri;
  }

  public void connect() throws ConnectException {
    /*        SocketAppender socketAppender = new SocketAppender();
    socketAppender.setPort(port);
    socketAppender.setRemoteHost(host);
    socketAppender.setIncludeCallerData(false);*/
    throw new ConnectException("failed to connect to " + targetURI);
  }

  @Override
  public void teardown() {}

  public boolean fallbackToConsole(Level level) {
    return false;
  }
}
