package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;

public class EpbContext {
  private final boolean isInner;
  private final TruffleLanguage.Env env;
  private @CompilerDirectives.CompilationFinal TruffleContext innerContext;

  public EpbContext(TruffleLanguage.Env env) {
    this.env = env;
    isInner = env.getConfig().get("isEpbInner") != null;
  }

  public void initialize() {
    if (!isInner) {
      innerContext = env.newContextBuilder().config("isEpbInner", "yes").build();
    }
  }

  public boolean isInner() {
    return isInner;
  }

  public TruffleContext getInnerContext() {
    return innerContext;
  }

  public TruffleLanguage.Env getEnv() {
    return env;
  }
}
