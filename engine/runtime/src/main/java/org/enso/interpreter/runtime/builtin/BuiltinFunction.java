package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.dsl.Owner;
import org.enso.interpreter.runtime.callable.function.Function;

/** BuiltinFunction encapsulates information about a builtin runtime function and its metadata. */
public class BuiltinFunction {
  // Note: ideally BuiltinFunction would be a record but there appears to be a bug in frgaal.
  private Function fun;
  private boolean isStatic;
  private Owner owner;

  public BuiltinFunction(Function fun, boolean isStatic, Owner owner) {
    this.fun = fun;
    this.isStatic = isStatic;
    this.owner = owner;
  }

  public boolean isStatic() {
    return this.isStatic;
  }

  /*public boolean isModuleOwner() {
    return this.owner == Owner.MODULE;
  }*/

  public Function getFunction() {
    return this.fun;
  }

  public String getFunctionName() {
    String fullName = fun.getName();
    return fullName.substring(fullName.lastIndexOf('.') + 1);
  }
}
