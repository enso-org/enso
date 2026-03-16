package org.enso.ydoc.server;

import java.io.IOException;
import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.slf4j.event.Level;

public final class Main {

  private Main() {}

  public static void main(String[] args) {
    // main method declaration is required to build the native library
  }

  public static AutoCloseable launch(
      String ydocHost,
      String ydocPort,
      YjsChannel.Server<String> jsonChannelCallbacks,
      YjsChannel.Server<Object> binaryChannelCallbacks,
      String logLevelName)
      throws IOException {
    var builder = Ydoc.builder();
    if (logLevelName != null) {
      builder.logLevel(Level.valueOf(logLevelName));
    }
    if (ydocHost != null) {
      builder.hostname(ydocHost);
    }
    if (ydocPort != null) {
      var port = Integer.parseInt(ydocPort);
      builder.port(port);
    }
    if (jsonChannelCallbacks != null) {
      builder.jsonChannelCallbacks(jsonChannelCallbacks);
    }
    if (binaryChannelCallbacks != null) {
      builder.binaryChannelCallbacks(binaryChannelCallbacks);
    }
    var hostAccess = WebEnvironment.defaultHostAccess;
    builder.hostAccess(hostAccess.build());
    var ydoc = builder.build();
    ydoc.start();
    return ydoc;
  }
}
