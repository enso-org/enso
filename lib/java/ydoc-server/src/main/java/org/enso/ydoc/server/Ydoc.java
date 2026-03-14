package org.enso.ydoc.server;

import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.polyfill.ParserPolyfill;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

public final class Ydoc implements AutoCloseable {
  static final Logger log = LoggerFactory.getLogger(Ydoc.class);

  private static final String YDOC_PATH = "ydoc.cjs";

  private final YdocScheduledExecutorService executor;
  private final ParserPolyfill parser;
  private final Context.Builder contextBuilder;
  private final String hostname;
  private final int port;
  private final YjsCallbacksSynchronized<String> jsonChannelCallbacks;
  private final YjsCallbacksSynchronized<Object> binaryChannelCallbacks;
  private final Level logLevel;

  private Context context;
  private ScheduledExecutorService statsLoggerExecutor;

  private Ydoc(
      YdocScheduledExecutorService executor,
      ParserPolyfill parser,
      Context.Builder contextBuilder,
      String hostname,
      int port,
      YjsCallbacksSynchronized<String> jsonChannelCallbacks,
      YjsCallbacksSynchronized<Object> binaryChannelCallbacks,
      Level logLevel) {
    this.executor = executor;
    this.parser = parser;
    this.contextBuilder = contextBuilder;
    this.hostname = hostname;
    this.port = port;
    this.jsonChannelCallbacks = jsonChannelCallbacks;
    this.binaryChannelCallbacks = binaryChannelCallbacks;
    this.logLevel = logLevel;
  }

  public static final class Builder {

    private static final String DEFAULT_HOSTNAME = "localhost";
    private static final int DEFAULT_PORT = 5976;

    private YdocScheduledExecutorService executor;
    private ParserPolyfill parser;
    private Context.Builder contextBuilder;
    private HostAccess.Builder hostAccessBuilder;
    private String hostname;
    private int port = -1;
    private Level logLevel = Level.ERROR;
    private YjsChannel.Server<String> jsonChannelCallbacks;
    private YjsChannel.Server<Object> binaryChannelCallbacks;

    private Builder() {}

    public Builder executor(YdocScheduledExecutorService executor) {
      this.executor = executor;
      return this;
    }

    public Builder parser(ParserPolyfill parser) {
      this.parser = parser;
      return this;
    }

    public Builder hostAccessBuilder(HostAccess.Builder hostAccessBuilder) {
      this.hostAccessBuilder = hostAccessBuilder;
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

    public Builder logLevel(Level logLevel) {
      this.logLevel = logLevel;
      return this;
    }

    public Builder jsonChannelCallbacks(YjsChannel.Server<String> callbacks) {
      this.jsonChannelCallbacks = callbacks;
      return this;
    }

    public Builder binaryChannelCallbacks(YjsChannel.Server<Object> callbacks) {
      this.binaryChannelCallbacks = callbacks;
      return this;
    }

    public Ydoc build() {
      if (executor == null) {
        final var debug = logLevel == Level.DEBUG || logLevel == Level.TRACE;
        executor = new YdocScheduledExecutorService(debug);
      }

      if (parser == null) {
        parser = new ParserPolyfill();
      }

      if (hostAccessBuilder == null) {
        hostAccessBuilder = WebEnvironment.defaultHostAccess;
      }

      if (contextBuilder == null) {
        contextBuilder =
            WebEnvironment.createContext(hostAccessBuilder.build()).allowIO(IOAccess.ALL);
      }

      if (hostname == null) {
        hostname = DEFAULT_HOSTNAME;
      }

      if (port == -1) {
        port = DEFAULT_PORT;
      }

      log.debug("Created Ydoc [{}, {}, {}]", logLevel, hostname, port);

      return new Ydoc(
          executor,
          parser,
          contextBuilder,
          hostname,
          port,
          new YjsCallbacksSynchronized<>(
              new DelegateYjsChannelCallbacks<>("JSON", jsonChannelCallbacks), executor),
          new YjsCallbacksSynchronized<>(
              new DelegateYjsChannelCallbacks<>("binary", binaryChannelCallbacks), executor),
          logLevel);
    }
  }

  public static Builder builder() {
    return new Builder();
  }

  public void start() throws IOException {
    var ydoc = Main.class.getResource(YDOC_PATH);
    if (ydoc == null) {
      throw new AssertionError(
          YDOC_PATH
              + " not found in resources. You probably need to first built it with: "
              + "`corepack pnpm -r compile`");
    }
    var ydocJs = Source.newBuilder("js", ydoc).build();

    // Submit initialization task
    var initFuture =
        executor.submit(
            () -> {
              var ctx = contextBuilder.build();
              WebEnvironment.initialize(ctx, executor.createHighPriorityView());
              parser.initialize(ctx);

              var bindings = ctx.getBindings("js");
              bindings.putMember("YDOC_HOST", hostname);
              bindings.putMember("YDOC_PORT", port);
              bindings.putMember("YDOC_JSON_CHANNEL_CALLBACKS", jsonChannelCallbacks);
              bindings.putMember("YDOC_BINARY_CHANNEL_CALLBACKS", binaryChannelCallbacks);
              bindings.putMember(
                  "YDOC_LS_DEBUG", logLevel == Level.DEBUG || logLevel == Level.TRACE);

              ctx.eval(ydocJs);

              return ctx;
            });

    runEventLoopUntil(initFuture::isDone);

    try {
      context = initFuture.get();
    } catch (Exception e) {
      throw new RuntimeException("Failed to initialize Ydoc", e);
    }

    if (logLevel == Level.DEBUG || logLevel == Level.TRACE) {
      statsLoggerExecutor =
          Executors.newSingleThreadScheduledExecutor(
              r -> {
                var t = new Thread(r, "Ydoc stats logger");
                t.setDaemon(true);
                return t;
              });
      statsLoggerExecutor.scheduleAtFixedRate(
          () -> {
            var stats = executor.getDebugStats();
            if (!stats.isEmpty()) {
              log.debug("{}", stats);
            }
          },
          30,
          30,
          TimeUnit.SECONDS);
    }

    runEventLoopBlocking();
  }

  /**
   * Runs the event loop until the given condition returns true.
   *
   * @param condition the condition to check; loop exits when it returns true
   */
  private void runEventLoopUntil(java.util.function.BooleanSupplier condition) {
    while (!condition.getAsBoolean() && !executor.isShutdown()) {
      executor.processPendingTasks();
      try {
        long delay = executor.getNextTaskDelayNanos();
        executor.waitForTasks(delay);
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        break;
      }
    }
  }

  /**
   * Runs the event loop continuously until {@link #close()} is called. This method blocks and
   * should typically be run in a dedicated thread.
   */
  public void runEventLoopBlocking() {
    runEventLoopUntil(() -> false);
  }

  @Override
  public void close() throws Exception {
    executor.shutdown();
    if (statsLoggerExecutor != null) {
      statsLoggerExecutor.shutdownNow();
    }
    if (context != null) {
      context.close(true);
    }
  }
}
