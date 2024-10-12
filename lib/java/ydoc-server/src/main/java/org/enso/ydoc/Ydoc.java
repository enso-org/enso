package org.enso.ydoc;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.enso.ydoc.polyfill.ParserPolyfill;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public final class Ydoc implements AutoCloseable {

  private static final String YDOC_EXECUTOR_THREAD_NAME = "Ydoc executor thread";
  private static final String YDOC_PATH = "ydoc.cjs";

  private final ScheduledExecutorService executor;
  private final ParserPolyfill parser;
  private final Context.Builder contextBuilder;
  private final String hostname;
  private final int port;

  private Context context;

  public Ydoc(
      ScheduledExecutorService executor,
      ParserPolyfill parser,
      Context.Builder contextBuilder,
      String hostname,
      int port) {
    this.executor = executor;
    this.parser = parser;
    this.contextBuilder = contextBuilder;
    this.hostname = hostname;
    this.port = port;
  }

  public static class Builder {

    private static final String DEFAULT_HOSTNAME = "localhost";
    private static final int DEFAULT_PORT = 1234;

    private ScheduledExecutorService executor;
    private ParserPolyfill parser;
    private Context.Builder contextBuilder;
    private String hostname;
    private int port = -1;

    private Builder() {}

    public Builder executor(ScheduledExecutorService executor) {
      this.executor = executor;
      return this;
    }

    public Builder parser(ParserPolyfill parser) {
      this.parser = parser;
      return this;
    }

    public Builder contextBuilder(Context.Builder contextBuilder) {
      this.contextBuilder = contextBuilder;
      return this;
    }

    public Builder hostname(String hostname) {
      this.hostname = hostname;
      return this;
    }

    public Builder port(int port) {
      this.port = port;
      return this;
    }

    public Ydoc build() {
      if (executor == null) {
        executor =
            Executors.newSingleThreadScheduledExecutor(
                r -> {
                  var t = new Thread(r);
                  t.setName(YDOC_EXECUTOR_THREAD_NAME);
                  return t;
                });
      }

      if (parser == null) {
        parser = new ParserPolyfill();
      }

      if (contextBuilder == null) {
        contextBuilder = WebEnvironment.createContext().allowIO(IOAccess.ALL);
      }

      if (hostname == null) {
        hostname = DEFAULT_HOSTNAME;
      }

      if (port == -1) {
        port = DEFAULT_PORT;
      }

      return new Ydoc(executor, parser, contextBuilder, hostname, port);
    }
  }

  public static Builder builder() {
    return new Builder();
  }

  public Context.Builder getContextBuilder() {
    return contextBuilder;
  }

  public void start() throws ExecutionException, InterruptedException, IOException {
    var ydoc = Main.class.getResource(YDOC_PATH);
    if (ydoc == null) {
      throw new AssertionError(
          YDOC_PATH
              + " not found in resources. You probably need to first built it with: "
              + "`corepack pnpm -r compile`");
    }
    var ydocJs = Source.newBuilder("js", ydoc).build();

    context =
        CompletableFuture.supplyAsync(
                () -> {
                  var ctx = contextBuilder.build();
                  WebEnvironment.initialize(ctx, executor);
                  parser.initialize(ctx);

                  var bindings = ctx.getBindings("js");
                  bindings.putMember("YDOC_HOST", hostname);
                  bindings.putMember("YDOC_PORT", port);
                  bindings.putMember("YDOC_LS_DEBUG", "false");

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
    if (context != null) {
      context.close(true);
    }
  }
}
