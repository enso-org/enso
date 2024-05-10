package org.enso.ydoc;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.enso.ydoc.polyfill.ParserPolyfill;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public final class Ydoc implements AutoCloseable {

  private static final String YDOC_SERVER_PATH = "/dist/assets/ydocServer.js";

  private final ExecutorService executor;
  private final ParserPolyfill parser;
  private final Context.Builder contextBuilder;

  private Context context;

  public Ydoc() {
    executor = Executors.newSingleThreadExecutor();
    parser = new ParserPolyfill();
    contextBuilder = WebEnvironment.createContext().allowIO(IOAccess.ALL);
  }

  public Context.Builder getContextBuilder() {
    return contextBuilder;
  }

  public void start() throws ExecutionException, InterruptedException, IOException {
    var ydoc = Main.class.getResource(YDOC_SERVER_PATH);
    if (ydoc == null) {
      throw new AssertionError(
          YDOC_SERVER_PATH
              + " not found in resources. You probably need to first built it with: "
              + "`npm --workspace=enso-gui2 run build-ydoc-server-polyglot`");
    }
    var ydocJs = Source.newBuilder("js", ydoc).mimeType("application/javascript+module").build();

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  WebEnvironment.initialize(ctx, executor);
                  parser.initialize(ctx);
                  ctx.eval(ydocJs);

                  return ctx;
                },
                executor)
            .get();
  }

  @Override
  public void close() throws Exception {
    executor.shutdownNow();
    executor.awaitTermination(3, TimeUnit.SECONDS);
    parser.close();
    if (context != null) {
      context.close(true);
    }
  }
}
