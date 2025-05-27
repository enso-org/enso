package org.enso.ydoc.server;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import org.enso.runner.common.WrongOption;
import org.enso.runner.common.YdocServerApi;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@org.openide.util.lookup.ServiceProvider(service = YdocServerApi.class)
public final class YdocServerImpl extends YdocServerApi {
  private static final Logger log = LoggerFactory.getLogger(YdocServerImpl.class);

  public YdocServerImpl() {}

  @Override
  protected AutoCloseable runYdocServer(String hostname, int port) throws WrongOption, IOException {
    try {
      var then = System.currentTimeMillis();
      var ydoc = Ydoc.builder().hostname(hostname).port(port).build();
      ydoc.start();
      var now = System.currentTimeMillis();
      log.warn("Ydoc server at {}:{} started in {} ms", hostname, port, now - then);
      return ydoc;
    } catch (ExecutionException | InterruptedException ex) {
      throw new IOException(ex);
    }
  }
}
