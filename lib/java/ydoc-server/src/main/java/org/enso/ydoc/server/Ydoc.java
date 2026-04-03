package org.enso.ydoc.server;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.polyfill.ParserPolyfill;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public final class Ydoc implements AutoCloseable {
  private static final System.Logger LOG = System.getLogger(Ydoc.class.getName());

  private static final String YDOC_PATH_ENV_NAME = "YDOC_SERVER_JS";
  private static final String YDOC_PATH = "ydoc.cjs";

  private final YdocScheduledExecutorService executor;
  private final ParserPolyfill parser;
  private final Context.Builder contextBuilder;
  private final String hostname;
  private final int port;
  private final YjsChannel.Server jsonChannelCallbacks;
  private final YjsChannel.Server binaryChannelCallbacks;

  private Context context;
  private ScheduledExecutorService statsLoggerExecutor;

  private Ydoc(
      YdocScheduledExecutorService executor,
      ParserPolyfill parser,
      Context.Builder contextBuilder,
      String hostname,
      int port,
      YjsChannel.Server jsonChannelCallbacks,
      YjsChannel.Server binaryChannelCallbacks) {
    this.executor = executor;
    this.parser = parser;
    this.contextBuilder = contextBuilder;
    this.hostname = hostname;
    this.port = port;
    this.jsonChannelCallbacks = jsonChannelCallbacks;
    this.binaryChannelCallbacks = binaryChannelCallbacks;
  }

  public static final class Builder {

    private static final String DEFAULT_HOSTNAME = "localhost";
    private static final int DEFAULT_PORT = 5976;

    private YdocScheduledExecutorService executor;
    private ParserPolyfill parser;
    private Context.Builder contextBuilder;
    private HostAccess hostAccess;
    private String hostname;
    private int port = -1;
    private YjsChannel.Server jsonChannelCallbacks;
    private YjsChannel.Server binaryChannelCallbacks;

    private Builder() {}

    public static final class DelegateConsumer<T> implements Consumer<T> {
      private final Consumer<T> delegate;

      DelegateConsumer(Consumer<T> delegate) {
        this.delegate = delegate;
      }

      @Override
      public void accept(T t) {
        LOG.log(System.Logger.Level.TRACE, "DelegateConsumer.accept[{0}]: {1}", t.getClass(), t);
        delegate.accept(t);
        LOG.log(System.Logger.Level.TRACE, "DelegateConsumer.accept finished");
      }
    }

    public static final class DelegateYjsChannel implements YjsChannel {
      private final YjsChannel delegate;

      DelegateYjsChannel(YjsChannel delegate) {
        this.delegate = delegate;
      }

      @Override
      public void send(Object o) {
        LOG.log(System.Logger.Level.TRACE, "DelegateYjsChannel.send[{0}]: {1}", o.getClass(), o);
        delegate.send(o);
        LOG.log(System.Logger.Level.TRACE, "DelegateYjsChannel.send finished");
      }

      @Override
      public void subscribe(Consumer<Object> cnsmr) {
        var wrap = new DelegateConsumer<Object>(cnsmr);
        LOG.log(
            System.Logger.Level.TRACE,
            "DelegateYjsChannel.subscribe[{0}]: {1}",
            cnsmr.getClass(),
            cnsmr);
        delegate.subscribe(wrap);
        LOG.log(System.Logger.Level.TRACE, "DelegateYjsChannel.subscribe finished");
      }

      public static final class Server implements YjsChannel.Server {
        private final String name;
        private final YjsChannel.Server delegate;

        Server(String name, YjsChannel.Server delegate) {
          this.name = name;
          this.delegate = delegate;
        }

        @HostAccess.Export
        @Override
        public void onConnect(YjsChannel channel) {
          LOG.log(
              System.Logger.Level.TRACE,
              "Enter onConnect[{0}] with {1} for {2}",
              name,
              channel,
              delegate);
          if (delegate != null) {
            var wrap = new DelegateYjsChannel(channel);
            delegate.onConnect(wrap);
          }
          LOG.log(System.Logger.Level.TRACE, "Exit onConnect[{0}] with {1}", name, channel);
        }
      }
    }

    public Builder executor(YdocScheduledExecutorService executor) {
      this.executor = executor;
      return this;
    }

    public Builder parser(ParserPolyfill parser) {
      this.parser = parser;
      return this;
    }

    public Builder hostAccess(HostAccess hostAccess) {
      this.hostAccess = hostAccess;
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

    public Builder jsonChannelCallbacks(YjsChannel.Server callbacks) {
      this.jsonChannelCallbacks = callbacks;
      return this;
    }

    public Builder binaryChannelCallbacks(YjsChannel.Server callbacks) {
      this.binaryChannelCallbacks = callbacks;
      return this;
    }

    public Ydoc build() {
      if (executor == null) {
        executor = new YdocScheduledExecutorService();
      }

      if (parser == null) {
        parser = new ParserPolyfill();
      }

      if (hostAccess == null) {
        hostAccess = WebEnvironment.defaultHostAccess.build();
      }

      if (contextBuilder == null) {
        contextBuilder = WebEnvironment.createContext(hostAccess).allowIO(IOAccess.ALL);
      }

      if (hostname == null) {
        hostname = DEFAULT_HOSTNAME;
      }

      if (port == -1) {
        port = DEFAULT_PORT;
      }

      var isTracing = LOG.isLoggable(System.Logger.Level.TRACE);
      LOG.log(System.Logger.Level.DEBUG, "Created Ydoc [{0}, {1}]", hostname, port);

      return new Ydoc(
          executor,
          parser,
          contextBuilder,
          hostname,
          port,
          isTracing
              ? new DelegateYjsChannel.Server("JSON", jsonChannelCallbacks)
              : jsonChannelCallbacks,
          isTracing
              ? new DelegateYjsChannel.Server("binary", binaryChannelCallbacks)
              : binaryChannelCallbacks);
    }
  }

  public static Builder builder() {
    return new Builder();
  }

  private YjsChannel.Server getJsonChannelCallbacksSynchronized() {
    return new YjsCallbacksSynchronized(jsonChannelCallbacks, executor);
  }

  private YjsChannel.Server getBinaryChannelCallbacksSynchronized() {
    return new YjsCallbacksSynchronized(binaryChannelCallbacks, executor);
  }

  private static String findYdocServerSrc() {
    var devYdocPath = System.getenv(YDOC_PATH_ENV_NAME);
    var assertsOn = false;
    assert assertsOn = true;
    if (devYdocPath != null) {
      if (assertsOn) {
        // asserts can be turned on with JAVA_TOOL_OPTIONS=-ea
        // or by compiling in native+test configuration via
        // ENSO_LAUNCHER=native,test sbt buildEngineDistribution
        return devYdocPath;
      }
      LOG.log(
          System.Logger.Level.WARNING,
          "Ignoring value {0} of {1} in non-testing environment",
          devYdocPath,
          YDOC_PATH_ENV_NAME);
    }
    return null;
  }

  public void start() throws IOException {
    var ydoc = Main.class.getResource(YDOC_PATH);

    var ydocDevJs = findYdocServerSrc();
    if (ydocDevJs != null) {
      var ydocFile = new File(ydocDevJs);
      if (!ydocFile.canRead()) {
        LOG.log(
            System.Logger.Level.WARNING,
            "Ignoring value {0} of {1} as the path cannot be read",
            ydocDevJs,
            YDOC_PATH_ENV_NAME);
      } else {
        ydoc = ydocFile.toURI().toURL();
        LOG.log(
            System.Logger.Level.INFO,
            "Loading {0} specified by {1} as Ydoc server script",
            ydoc,
            YDOC_PATH_ENV_NAME);
        // enabling Google Dev Tools debugging of YDOC_PATH_ENV_NAME
        contextBuilder.option("inspect", "true");
        contextBuilder.option("inspect.Suspend", "" + ydocDevJs.contains("suspend"));
        contextBuilder.option("inspect.Path", "enso_ydoc");
      }
    }

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
              bindings.putMember(
                  "YDOC_JSON_CHANNEL_CALLBACKS", getJsonChannelCallbacksSynchronized());
              bindings.putMember(
                  "YDOC_BINARY_CHANNEL_CALLBACKS", getBinaryChannelCallbacksSynchronized());
              var isDebug = LOG.isLoggable(System.Logger.Level.DEBUG);
              bindings.putMember("YDOC_LS_DEBUG", isDebug);

              ctx.eval(ydocJs);

              return ctx;
            });

    runEventLoopUntil(initFuture::isDone);

    try {
      context = initFuture.get();
    } catch (Exception e) {
      var msg = "Failed to initialize Ydoc";
      LOG.log(System.Logger.Level.ERROR, msg, e);
      if (ydocDevJs != null) {
        final var failureInYdocDevJs = 7;
        System.exit(failureInYdocDevJs);
      } else {
        throw new IllegalStateException(msg, e);
      }
    }

    if (LOG.isLoggable(System.Logger.Level.TRACE)) {
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
              LOG.log(System.Logger.Level.TRACE, "{0}", stats);
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
