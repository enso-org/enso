package org.enso.logging;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.joran.JoranConfigurator;
import ch.qos.logback.classic.net.SimpleSocketServer;
import ch.qos.logback.core.joran.spi.JoranException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import org.slf4j.event.Level;

class LoggingServer extends LoggingService {

  private int port;
  private SimpleSocketServer logServer;

  public LoggingServer(int port) {
    this.port = port;
    this.logServer = null;
  }

  public URI start(Level level, Path path, String prefix, String appenderName)
      throws URISyntaxException, JoranException {
    var lc = new LoggerContext();
    var configurator = new JoranConfigurator();
    System.setProperty("logging-server.logRoot", path.toAbsolutePath().toString());
    System.setProperty("logging-server.logPrefix", prefix);
    System.setProperty("logging-server.logLevel", level.toString().toLowerCase());
    System.setProperty("logging-server.appender", appenderName);
    configurator.setContext(lc);
    configurator.doConfigure(this.getClass().getResourceAsStream("/logging-server.logback.xml"));
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
