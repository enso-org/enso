package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;
import java.util.logging.Level;

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
  private @CompilationFinal GuardedTruffleContext innerContext;
  private final ReentrantLock lock = new ReentrantLock();

  /**
   * Creates a new instance of this context.
   *
   * @param env the current language environment.
   */
  EpbContext(TruffleLanguage.Env env) {
    this.env = env;
    isInner = env.getConfig().get(INNER_OPTION) != null;
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
            new GuardedTruffleContext(
                env.newInnerContextBuilder()
                    .initializeCreatorContext(true)
                    .inheritAllAccess(true)
                    .config(INNER_OPTION, "yes")
                    .build(),
                true);
      }
      initializeLanguages(env, innerContext, preInitializeLanguages);
    }
  }

  private static void initializeLanguages(
      TruffleLanguage.Env environment, GuardedTruffleContext innerContext, String langs) {
    if (langs == null || langs.isEmpty()) {
      return;
    }
    var log = environment.getLogger(EpbContext.class);
    log.log(
        Level.INFO,
        "Initializing languages {0}. In thread {1}",
        new Object[] {langs, Thread.currentThread().getName()});
    var cdl = new CountDownLatch(1);
    var run =
        (Consumer<TruffleContext>)
            (context) -> {
              var epbCtx = EpbContext.get(null);
              var lock = epbCtx.getLock();
              var beforeEnter = innerContext.enter(null);
              lock.lock();
              try {
                log.log(
                    Level.INFO,
                    "Entering initialization thread '{0}'",
                    Thread.currentThread().getName());
                cdl.countDown();
                for (var l : langs.split(",")) {
                  log.log(Level.FINEST, "Initializing language {0}", l);
                  long then = System.currentTimeMillis();
                  var res = context.initializeInternal(null, l);
                  long took = System.currentTimeMillis() - then;
                  log.log(
                      Level.FINE,
                      "Done initializing language {0} with {1} in {2} ms",
                      new Object[] {l, res, took});
                }
              } finally {
                lock.unlock();
                innerContext.leave(null, beforeEnter);
              }
            };
    var init = innerContext.createThread(environment, run);
    log.log(Level.INFO, "Starting initialization thread '{0}'", init.getName());
    init.start();
    try {
      cdl.await();
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
    }
    log.log(Level.FINEST, "Initializing on background");
  }

  /**
   * @param node the location of context access. Pass {@code null} if not in a node.
   * @return the proper context instance for the current {@link
   *     com.oracle.truffle.api.TruffleContext}.
   */
  public static EpbContext get(Node node) {
    return REFERENCE.get(node);
  }

  /** @return the language environment associated with this context. */
  public TruffleLanguage.Env getEnv() {
    return env;
  }

  public ReentrantLock getLock() {
    return lock;
  }

  public GuardedTruffleContext getInnerContext() {
    return innerContext;
  }
}
