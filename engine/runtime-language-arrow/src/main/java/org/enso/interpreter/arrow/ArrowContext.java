package org.enso.interpreter.arrow;

import com.oracle.truffle.api.TruffleLanguage;

public class ArrowContext {
  private final TruffleLanguage.Env env;

  public ArrowContext(TruffleLanguage.Env env) {
    this.env = env;
  }

  public void initialize() {
    //
  }
}
