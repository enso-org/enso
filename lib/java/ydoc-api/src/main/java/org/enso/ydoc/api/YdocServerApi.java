package org.enso.ydoc.api;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ServiceLoader;
import org.slf4j.event.Level;

public abstract class YdocServerApi {
  /**
   * Initializes the Ydoc subsystem.
   *
   * @param hostname hostname to bind to
   * @param port port to bind to
   * @param jsonServer implementation handling JSON messages
   * @param binaryServer implementation handling binary messages communication
   * @param logLevel
   * @return
   * @throws IOException
   * @throws URISyntaxException
   */
  public static AutoCloseable launchYdocServer(
      String hostname,
      int port,
      YjsChannel.Server jsonServer,
      YjsChannel.Server binaryServer,
      Level logLevel)
      throws IOException, URISyntaxException {
    var loader = YdocServerApi.class.getClassLoader();
    var it = ServiceLoader.load(YdocServerApi.class, loader).iterator();
    if (!it.hasNext()) {
      throw new IllegalStateException("No Ydoc server implementation found");
    }
    var impl = it.next();
    return impl.runYdocServer(hostname, port, jsonServer, binaryServer, logLevel);
  }

  protected abstract AutoCloseable runYdocServer(
      String hostname,
      int port,
      YjsChannel.Server jsonChannelCallbacks,
      YjsChannel.Server binaryChannelCallbacks,
      Level logLevel)
      throws IOException, URISyntaxException;
}
