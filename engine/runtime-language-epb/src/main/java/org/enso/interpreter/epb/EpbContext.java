package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.nodes.Node;
import java.util.concurrent.locks.ReentrantLock;
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
  private @CompilationFinal TruffleContext innerContext;
  private final ReentrantLock lock = new ReentrantLock();
  private final TruffleLogger log;

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

  /** @return the language environment associated with this context. */
  public TruffleLanguage.Env getEnv() {
    return env;
  }

  public TruffleContext getInnerContext() {
    return innerContext;
  }

  public void log(Level level, String msg, Object... args) {
    this.log.log(level, msg, args);
  }
}
