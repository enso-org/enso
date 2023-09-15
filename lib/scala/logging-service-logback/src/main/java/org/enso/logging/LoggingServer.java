package org.enso.logging;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.net.SimpleSocketServer;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import org.enso.logger.LogbackSetup;
import org.enso.logger.config.Appender;
import org.slf4j.event.Level;

class LoggingServer extends LoggingService<URI> {

  private int port;
  private SimpleSocketServer logServer;

  public LoggingServer(int port) {
    this.port = port;
    this.logServer = null;
  }

  public URI start(Level level, Path path, String prefix, Appender appender) {
    var lc = new LoggerContext();
    var setup = LogbackSetup.forContext(lc, appender);

    logServer = new SimpleSocketServer(lc, port);
    logServer.start();
    try {
      setup.setup(level, path, prefix, setup.getConfig());
      return new URI(null, null, "localhost", port, null, null, null);
    } catch (URISyntaxException e) {
      throw new RuntimeException(e);
    }
  }

  public boolean isSetup() {
    return logServer != null;
  }

  @Override
  public void teardown() {
    if (logServer != null) {
      logServer.close();
    }
  }
}
