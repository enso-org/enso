package org.enso.ydoc.api;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ServiceLoader;

public abstract class YdocServerApi {

  public static AutoCloseable launchYdocServer(
      String hostname,
      int port,
      YjsChannelCallbacks jsonChannelCallbacks,
      YjsChannelCallbacks binaryChannelCallbacks)
      throws IOException, URISyntaxException {
    var loader = YdocServerApi.class.getClassLoader();
    var it = ServiceLoader.load(YdocServerApi.class, loader).iterator();
    if (!it.hasNext()) {
      throw new IllegalStateException("No Ydoc server implementation found");
    }
    var impl = it.next();
    return impl.runYdocServer(hostname, port, jsonChannelCallbacks, binaryChannelCallbacks);
  }

  protected abstract AutoCloseable runYdocServer(
      String hostname,
      int port,
      YjsChannelCallbacks jsonChannelCallbacks,
      YjsChannelCallbacks binaryChannelCallbacks)
      throws IOException, URISyntaxException;
}
