package org.enso.ydoc.server;

import java.io.IOException;
import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.api.YjsChannelCallbacks;
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
      YjsChannelCallbacks jsonChannelCallbacks,
      YjsChannelCallbacks binaryChannelCallbacks,
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
    var hostAccess =
        WebEnvironment.defaultHostAccess
            // allowImplementations is required to call methods on JS objects from Java, i.e. to
            // call methods on `YjsChannel` object returned from JS
            .allowImplementations(YjsChannel.class)
            .allowPublicAccess(true);
    builder.hostAccessBuilder(hostAccess);
    var ydoc = builder.build();
    ydoc.start();
    return ydoc;
  }
}
