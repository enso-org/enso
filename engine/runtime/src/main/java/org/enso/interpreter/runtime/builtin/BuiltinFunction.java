package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.runtime.callable.function.Function;

/** BuiltinFunction encapsulates information about a builtin runtime function and its metadata. */
public class BuiltinFunction {
  // Note: ideally BuiltinFunction would be a record but there appears to be a bug in frgaal.
  private Function fun;
  private boolean isStatic;
  private boolean autoRegister;

  public BuiltinFunction(Function fun, boolean isStatic, boolean autoRegister) {
    this.fun = fun;
    this.isStatic = isStatic;
    this.autoRegister = autoRegister;
  }

  public boolean isStatic() {
    return this.isStatic;
  }

  public Function getFunction() {
    return this.fun;
  }

  public String getFunctionName() {
    String fullName = fun.getName();
    return fullName.substring(fullName.lastIndexOf('.') + 1);
  }
}
