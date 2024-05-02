package org.enso.ydoc;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.enso.ydoc.polyfill.ParserPolyfill;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public class Main {

  private static final String YDOC_SERVER_PATH = "/dist/assets/ydocServer.js";

  private Main() {}

  public static void main(String[] args) throws Exception {
    System.setProperty(
        "helidon.serialFilter.pattern",
        "javax.management.**;java.lang.**;java.rmi.**;javax.security.auth.Subject;!*");

    var ydoc = Main.class.getResource(YDOC_SERVER_PATH);
    var contextBuilder = WebEnvironment.createContext().allowIO(IOAccess.ALL);

    Sampling.init();

    // Can't use try-with-resource in ExecutorService because API was added in JDK19
    var executor = Executors.newSingleThreadExecutor();
    try {
      var parser = new ParserPolyfill();
      var ydocJs = Source.newBuilder("js", ydoc).mimeType("application/javascript+module").build();

      CompletableFuture.supplyAsync(contextBuilder::build, executor)
          .thenAcceptAsync(
              ctx -> {
                WebEnvironment.initialize(ctx, executor);
                parser.initialize(ctx);

                ctx.eval(ydocJs);
              },
              executor)
          .get();

      System.out.println("Press enter to exit");
      System.in.read();
    } finally {
      executor.shutdown();
      var terminated = executor.awaitTermination(10, TimeUnit.SECONDS);
      if (!terminated) {
        executor.shutdownNow();
      }
    }
  }
}
