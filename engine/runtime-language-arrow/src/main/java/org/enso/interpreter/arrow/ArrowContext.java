package org.enso.interpreter.arrow;

import com.oracle.truffle.api.TruffleLanguage;

final class ArrowContext {
  private final TruffleLanguage.Env env;

  public ArrowContext(TruffleLanguage.Env env) {
    this.env = env;
  }

  public void initialize() {
    //
  }
}
