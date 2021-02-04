package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;

/**
 * A context for {@link EpbLanguage}. Provides access to both isolated Truffle contexts used in
 * polyglot execution.
 */
public class EpbContext {
  private static final String INNER_OPTION = "isEpbInner";
  private final boolean isInner;
  private final TruffleLanguage.Env env;
  private @CompilerDirectives.CompilationFinal GuardedTruffleContext innerContext;
  private final GuardedTruffleContext currentContext;

  /**
   * Creates a new instance of this context.
   *
   * @param env the current language environment.
   */
  public EpbContext(TruffleLanguage.Env env) {
    this.env = env;
    isInner = env.getConfig().get(INNER_OPTION) != null;
    currentContext = new GuardedTruffleContext(env.getContext(), isInner);
  }

  /**
   * Initializes the context. No-op in the inner context. Spawns the inner context if called from
   * the outer context.
   */
  public void initialize() {
    if (!isInner) {
      innerContext =
          new GuardedTruffleContext(
              env.newContextBuilder().config(INNER_OPTION, "yes").build(), true);
    }
  }

  /**
   * Checks if this context corresponds to the inner Truffle context.
   *
   * @return true if run in the inner Truffle context, false otherwise.
   */
  public boolean isInner() {
    return isInner;
  }

  /**
   * @return the inner Truffle context handle if called from the outer context, or null if called in
   *     the inner context.
   */
  public GuardedTruffleContext getInnerContext() {
    return innerContext;
  }

  /** @return returns the currently entered Truffle context handle. */
  public GuardedTruffleContext getCurrentContext() {
    return currentContext;
  }

  /** @return the language environment associated with this context. */
  public TruffleLanguage.Env getEnv() {
    return env;
  }
}
