package org.enso.logger.config;

import com.typesafe.config.Config;
import java.net.URI;

public class SocketAppender extends Appender {
  private String name;
  private String hostname;

  public String getHostname() {
    return hostname;
  }

  public int getPort() {
    return port;
  }

  private int port;
  private int reconnectionDelay;
  private boolean includeCallerData;

  private SocketAppender(String hostname, int port, Config config) {
    super(config);
    this.name = "socket";
    this.hostname = hostname;
    this.port = port;
    this.reconnectionDelay = 10000;
    this.includeCallerData = true;
  }

  @Override
  public String getName() {
    return name;
  }

  public boolean isSameTarget(URI uri) {
    return uri.getHost().equals(hostname) && uri.getPort() == port;
  }

  public static Appender parse(Config config) {
    return new SocketAppender(config.getString("hostname"), config.getInt("port"), config);
  }
}
