package org.enso.runner.common;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ServiceLoader;

public abstract class YdocServerApi {
  public static AutoCloseable launchYdocServer(String hostname, int port)
      throws WrongOption, IOException, URISyntaxException {
    var loader = YdocServerApi.class.getClassLoader();
    var it = ServiceLoader.load(YdocServerApi.class, loader).iterator();
    if (!it.hasNext()) {
      throw new WrongOption("No Ydoc server implementation found");
    }
    var impl = it.next();
    return impl.runYdocServer(hostname, port);
  }

  protected abstract AutoCloseable runYdocServer(String hostname, int port)
      throws WrongOption, IOException, URISyntaxException;
}
