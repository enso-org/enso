package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.net.URL;
import java.util.Random;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.Function;
import java.util.logging.Level;
import org.enso.runtime.utils.ThreadUtils;
import org.enso.ydoc.polyfill.ParserPolyfill;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Value;

/**
 * A context for {@link EpbLanguage}. Provides access to both isolated Truffle contexts used in
 * polyglot execution.
 */
final class EpbContext {

  private static final TruffleLanguage.ContextReference<EpbContext> REFERENCE =
      TruffleLanguage.ContextReference.create(EpbLanguage.class);

  private static final String INNER_OPTION = "isEpbInner";
  private final boolean isInner;
  private final TruffleLanguage.Env env;
  private @CompilationFinal TruffleContext innerContext;
  private final TruffleLogger log;
  private final Random delayer = new Random();
  private Future<Void> polyfillInitialized;

  /**
   * Creates a new instance of this context.
   *
   * @param env the current language environment.
   */
  EpbContext(TruffleLanguage.Env env) {
    this.env = env;
    isInner = env.getConfig().get(INNER_OPTION) != null;
    this.log = env.getLogger(EpbContext.class);
  }

  /**
   * Initializes the context.No-op in the inner context. Spawns the inner context if called from the
   * outer context. Shielded against double initialization.
   *
   * @param preInitializeLanguages comma separated list of languages to immediately initialize
   */
  public void initialize(String preInitializeLanguages) {
    if (!isInner) {
      if (innerContext == null) {
        innerContext =
            env.newInnerContextBuilder()
                .initializeCreatorContext(true)
                .inheritAllAccess(true)
                .threadAccessDeniedHandler(this::handleMultiAccess)
                .config(INNER_OPTION, "yes")
                .build();
      }
    }
  }

  /**
   * @param node the location of context access. Pass {@code null} if not in a node.
   * @return the proper context instance for the current {@link
   *     com.oracle.truffle.api.TruffleContext}.
   */
  public static EpbContext get(Node node) {
    return REFERENCE.get(node);
  }

  /**
   * @return the language environment associated with this context.
   */
  public TruffleLanguage.Env getEnv() {
    return env;
  }

  public TruffleContext getInnerContext() {
    return innerContext;
  }

  public void log(Level level, String msg, Object... args) {
    this.log.log(level, msg, args);
  }

  final Void initializePolyfill(Node node, TruffleContext ctx) {
    Runnable toInit = null;
    synchronized (this) {
      if (polyfillInitialized == null) {
        var cf = new CompletableFuture<Void>();
        toInit = createPolyfillSetup(cf, node, ctx);
        polyfillInitialized = cf;
      }
    }
    if (toInit != null) {
      toInit.run();
    }
    for (; ; ) {
      try {
        return polyfillInitialized.get();
      } catch (InterruptedException | ExecutionException ex) {
        // log and try again
        this.log.log(Level.INFO, null, ex);
      }
    }
  }

  private Runnable createPolyfillSetup(
      CompletableFuture<Void> whenDone, Node node, TruffleContext ctx) {
    var ensoLanguage = getEnv().getInternalLanguages().get("enso");
    var exec = getEnv().lookup(ensoLanguage, ScheduledExecutorService.class);
    assert exec != null : "Need executor from " + ensoLanguage;
    Function<URL, Value> eval =
        (url) -> {
          try {
            var src = Source.newBuilder("js", url).build();
            var obj = ctx.evalPublic(node, src);
            return Value.asValue(obj);
          } catch (IOException ex) {
            throw raise(RuntimeException.class, ex);
          }
        };
    return () -> {
      try {
        WebEnvironment.initialize(eval, exec);
        var parserPolyfill = new ParserPolyfill();
        parserPolyfill.initialize(eval);
        whenDone.complete(null);
      } catch (Exception ex) {
        whenDone.completeExceptionally(ex);
      }
    };
  }

  final void handleMultiAccess(String msg) {
    try {
      var ms = delayer.nextInt(10, 1000);
      // dump stack when assertions on
      assert dumpStack(msg, ms);
      Thread.sleep(ms);
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
    }
  }

  private boolean dumpStack(String msg, int ms) {
    var prefix = "[PolyglotAccess:" + Thread.currentThread().getName() + "]";
    var dump = ThreadUtils.dumpAllStacktraces("[epb] ", prefix);
    log(Level.WARNING, msg);
    log(Level.FINE, dump);
    log(Level.INFO, "Waiting " + ms + " ms");
    return true;
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
