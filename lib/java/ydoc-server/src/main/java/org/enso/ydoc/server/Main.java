package org.enso.ydoc.server;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import org.enso.ydoc.api.MessageCallbacks;
import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.polyfill.web.WebEnvironment;

public final class Main {

  private Main() {}

  public static AutoCloseable launch(
      String ydocHost,
      String ydocPort,
      MessageCallbacks jsonChannelCallbacks,
      MessageCallbacks binaryChannelCallbacks)
      throws IOException {
    try {
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
    } catch (ExecutionException | InterruptedException ex) {
      throw new IOException(ex);
    }
  }
}
