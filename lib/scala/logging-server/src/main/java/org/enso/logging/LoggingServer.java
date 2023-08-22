package org.enso.logging;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.net.SimpleSocketServer;
import ch.qos.logback.core.joran.spi.JoranException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import org.enso.logger.LogbackSetup;
import org.enso.logger.config.Appender;
import org.slf4j.event.Level;

class LoggingServer extends LoggingService {

  private int port;
  private SimpleSocketServer logServer;

  public LoggingServer(int port) {
    this.port = port;
    this.logServer = null;
  }

  public URI start(Level level, Path path, String prefix, Appender appender)
      throws URISyntaxException, JoranException {
    var lc = new LoggerContext();
    var setup = LogbackSetup.forContext(lc, appender);
    setup.setup(level, path, prefix, setup.getConfig());
    logServer = new SimpleSocketServer(lc, port);
    logServer.start();
    return new URI(null, null, "localhost", port, null, null, null);
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
