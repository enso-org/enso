package org.enso.ydoc;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import org.enso.profiling.sampler.OutputStreamSampler;
import org.enso.ydoc.polyfill.ParserPolyfill;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public class Main {

  private static final String YDOC_SERVER_PATH = "/dist/assets/ydocServer.js";
  private static final String SAMPLES_PATH = "/tmp/ydoc-server.out";

  private Main() {}

  public static void main(String[] args) throws Exception {
    var ydoc = Main.class.getResource(YDOC_SERVER_PATH);
    var contextBuilder = WebEnvironment.createContext().allowIO(IOAccess.ALL);
    var sampler = OutputStreamSampler.ofFile(new File(SAMPLES_PATH));
    sampler.start();
    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  try {
                    sampler.stop();
                  } catch (IOException e) {
                    throw new RuntimeException(e);
                  }
                }));

    try (var executor = Executors.newSingleThreadExecutor();
        var parser = new ParserPolyfill()) {
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
    }
  }
}
