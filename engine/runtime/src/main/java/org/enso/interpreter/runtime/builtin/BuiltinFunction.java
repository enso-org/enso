package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.runtime.callable.function.Function;

/** BuiltinFunction encapsulates information about a builtin runtime function and its metadata. */
public class BuiltinFunction {
  // Note: ideally BuiltinFunction would be a record but there appears to be a bug in frgaal.
  private Function fun;
  private boolean autoRegister;

  public BuiltinFunction(Function fun, boolean autoRegister) {
    this.fun = fun;
    this.autoRegister = autoRegister;
  }

  public boolean isAutoRegister() {
    return this.autoRegister;
  }

  public Function getFunction() {
    return this.fun;
  }
}
