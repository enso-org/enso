package org.enso.logging;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.joran.JoranConfigurator;
import ch.qos.logback.classic.net.SimpleSocketServer;
import ch.qos.logback.core.joran.spi.JoranException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import org.slf4j.event.Level;

class ForwardToServer extends LoggingService {
  private static final String rollingFileAppender = "rolling-file";
  private static final String consoleAppender = "console";
  private static final String socketForwarderAppender = "socket-forwarder";

  private int port;
  private SimpleSocketServer logServer;

  public ForwardToServer(int port) {
    this.port = port;
    this.logServer = null;
  }

  public URI logToFile(Level level, Path path, String prefix)
      throws URISyntaxException, JoranException {
    var lc = new LoggerContext();
    var configurator = new JoranConfigurator();
    System.setProperty("logging-server.logRoot", path.toAbsolutePath().toString());
    System.setProperty("logging-server.logPrefix", prefix);
    System.setProperty("logging-server.logLevel", level.toString().toLowerCase());
    System.setProperty("logging-server.appender", rollingFileAppender);
    configurator.setContext(lc);
    configurator.doConfigure(this.getClass().getResourceAsStream("/logging-server.logback.xml"));
    logServer = new SimpleSocketServer(lc, port);
    logServer.start();
    return new URI(null, null, "localhost", port, null, null, null);
  }

  public void forwardToSocket(Level level, String targetHostname, int targetPort)
      throws JoranException {
    var lc = new LoggerContext();
    var configurator = new JoranConfigurator();
    configurator.setContext(lc);
    System.setProperty("logging-server.targetHost", targetHostname);
    System.setProperty("logging-server.targetPort", String.valueOf(targetPort));
    System.setProperty("logging-server.logLevel", level.toString().toLowerCase());
    System.setProperty("logging-server.appender", socketForwarderAppender);
    configurator.doConfigure(this.getClass().getResourceAsStream("/logging-server.logback.xml"));
    logServer = new SimpleSocketServer(lc, port);
    logServer.start();
  }

  public void logToConsole(Level level) throws JoranException {
    var lc = new LoggerContext();
    var configurator = new JoranConfigurator();
    configurator.setContext(lc);
    System.setProperty("logging-server.logLevel", level.toString().toLowerCase());
    System.setProperty("logging-server.appender", consoleAppender);
    configurator.doConfigure(this.getClass().getResourceAsStream("/logging-server.logback.xml"));
    logServer = new SimpleSocketServer(lc, port);
    logServer.start();
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
