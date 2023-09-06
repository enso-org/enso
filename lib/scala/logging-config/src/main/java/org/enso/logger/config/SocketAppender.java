package org.enso.logger.config;

import com.typesafe.config.Config;
import org.enso.logger.LoggerSetup;
import org.slf4j.event.Level;

/** Config for log configuration that forwards logs to the network socket as-is. */
public final class SocketAppender extends Appender {
  private String name;
  private String host;

  /**
   * Returns the name of the host of the network socket to connect to.
   *
   * @return
   */
  public String getHost() {
    return host;
  }

  /** Returns the port of the network socket to connect to. */
  public int getPort() {
    return port;
  }

  private int port;

  /** Returns the number of miliseconds after a failed connection should be re-established. */
  public int getReconnectionDelay() {
    return reconnectionDelay;
  }

  private final int reconnectionDelay;

  private SocketAppender(String host, int port, int reconnectionDelay) {
    this.name = appenderName;
    this.host = host;
    this.port = port;
    this.reconnectionDelay = reconnectionDelay;
  }

  @Override
  public String getName() {
    return name;
  }

  public static Appender parse(Config config) throws MissingConfigurationField {
    if (!config.hasPath(hostKey)) throw new MissingConfigurationField(hostKey);
    if (!config.hasPath(portKey)) throw new MissingConfigurationField(portKey);
    int reconnectionDelay =
        config.hasPath(reconnectionDelayKey) ? config.getInt(reconnectionDelayKey) : 10000;
    return new SocketAppender(config.getString(hostKey), config.getInt(portKey), reconnectionDelay);
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup loggerSetup) {
    return loggerSetup.setupSocketAppender(logLevel, host, port);
  }

  @Override
  public boolean setupForURI(Level logLevel, String host, int port, LoggerSetup loggerSetup) {
    return loggerSetup.setupSocketAppender(logLevel, host, port);
  }

  private static final String hostKey = "hostname";
  private static final String portKey = "port";
  private static final String reconnectionDelayKey = "reconnection-delay";
  public static final String appenderName = "socket";
}
