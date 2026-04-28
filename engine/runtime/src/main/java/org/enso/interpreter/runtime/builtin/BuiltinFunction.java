package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.runtime.callable.function.Function;

/** BuiltinFunction encapsulates information about a builtin runtime function and its metadata. */
public class BuiltinFunction {
  // Note: ideally BuiltinFunction would be a record but there appears to be a bug in frgaal.
  private Function fun;

  public BuiltinFunction(Function fun) {
    this.fun = fun;
  }

  public Function getFunction() {
    return this.fun;
  }
}
