package org.enso.ydoc.server;

import java.io.IOException;
import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.polyfill.web.WebEnvironment;

public final class Main {

  private Main() {}

  public static void main(String[] args) {
    // main method declaration is required to build the native library
  }

  public static AutoCloseable launch(
      String ydocHost,
      String ydocPort,
      YjsChannel.Server jsonChannelCallbacks,
      YjsChannel.Server binaryChannelCallbacks,
      YjsChannel.Server visControlChannelCallbacks,
      YjsChannel.Server visDataChannelCallbacks)
      throws IOException {
    var builder = Ydoc.builder();
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
    if (visControlChannelCallbacks != null) {
      builder.visControlChannelCallbacks(visControlChannelCallbacks);
    }
    if (visDataChannelCallbacks != null) {
      builder.visDataChannelCallbacks(visDataChannelCallbacks);
    }
    var hostAccess =
        WebEnvironment.defaultHostAccess
            // allowImplementations is required to call methods on JS objects from Java, i.e. to
            // call methods on `YjsChannel` object returned from JS
            .allowImplementations(YjsChannel.class)
            .allowPublicAccess(true);
    builder.hostAccess(hostAccess.build());
    var ydoc = builder.build();
    ydoc.start();
    return ydoc;
  }
}
