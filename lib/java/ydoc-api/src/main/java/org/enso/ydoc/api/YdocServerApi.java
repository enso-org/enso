package org.enso.ydoc.api;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ServiceLoader;

public abstract class YdocServerApi {
  /**
   * Initializes the Ydoc subsystem.
   *
   * @param hostname hostname to bind to
   * @param port port to bind to
   * @param jsonServer implementation handling JSON messages
   * @param binaryServer implementation handling binary messages communication
   * @return
   * @throws IOException
   * @throws URISyntaxException
   */
  public static AutoCloseable launchYdocServer(
      String hostname, int port, YjsChannel.Server jsonServer, YjsChannel.Server binaryServer)
      throws IOException, URISyntaxException {
    var loader = YdocServerApi.class.getClassLoader();
    var it = ServiceLoader.load(YdocServerApi.class, loader).iterator();
    if (!it.hasNext()) {
      throw new IllegalStateException("No Ydoc server implementation found");
    }
    var impl = it.next();
    return impl.runYdocServer(hostname, port, jsonServer, binaryServer);
  }

  protected abstract AutoCloseable runYdocServer(
      String hostname,
      int port,
      YjsChannel.Server jsonChannelCallbacks,
      YjsChannel.Server binaryChannelCallbacks)
      throws IOException, URISyntaxException;
}
