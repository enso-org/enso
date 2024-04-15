package org.enso.ydoc;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import org.enso.ydoc.polyfill.Platform;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public class Main {

  private static final String YDOC_SERVER_PATH = "/dist/assets/ydocServer.js";

  private Main() {}

  public static void main(String[] args) throws Exception {
    var ydoc = Main.class.getResource(YDOC_SERVER_PATH);

    HostAccess hostAccess =
        HostAccess.newBuilder(HostAccess.EXPLICIT)
            .allowArrayAccess(true)
            .allowBufferAccess(true)
            .build();

    var b =
        Context.newBuilder("js")
            .allowIO(IOAccess.ALL)
            .allowHostAccess(hostAccess)
            .allowExperimentalOptions(true);
    var chromePort = Integer.getInteger("inspectPort", -1);
    if (chromePort > 0) {
      b.option("inspect", ":" + chromePort);
    }

    try (var executor = Executors.newSingleThreadExecutor()) {
      var ydocJs = Source.newBuilder("js", ydoc).mimeType("application/javascript+module").build();

      CompletableFuture.supplyAsync(b::build, executor)
          .thenAcceptAsync(
              ctx -> {
                Platform.initialize(ctx, executor);
                ctx.eval(ydocJs);
              },
              executor)
          .get();

      System.out.println("Press enter to exit");
      System.in.read();
    }
  }
}
