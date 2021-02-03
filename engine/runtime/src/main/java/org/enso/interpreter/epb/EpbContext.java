package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;

public class EpbContext {
  private final boolean isInner;
  private final TruffleLanguage.Env env;
  private @CompilerDirectives.CompilationFinal
  GuardedTruffleContext innerContext;
  private final GuardedTruffleContext currentContext;

  public EpbContext(TruffleLanguage.Env env) {
    this.env = env;
    isInner = env.getConfig().get("isEpbInner") != null;
    currentContext = new GuardedTruffleContext(env.getContext(), isInner);
  }

  public void initialize() {
    if (!isInner) {
      innerContext =
          new GuardedTruffleContext(
              env.newContextBuilder().config("isEpbInner", "yes").build(), true);
    }
  }

  public boolean isInner() {
    return isInner;
  }

  public GuardedTruffleContext getInnerContext() {
    return innerContext;
  }

  public GuardedTruffleContext getCurrentContext() {
    return currentContext;
  }

  public TruffleLanguage.Env getEnv() {
    return env;
  }
}
