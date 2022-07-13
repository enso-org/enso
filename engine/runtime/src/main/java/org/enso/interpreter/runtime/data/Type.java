package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.runtime.scope.ModuleScope;

public class Type implements TruffleObject {
  private final String name;
  private final ModuleScope definitionScope;

  public Type(String name, ModuleScope definitionScope) {
    this.name = name;
    this.definitionScope = definitionScope;
  }

  public String getName() {
    return name;
  }

  public ModuleScope getDefinitionScope() {
    return definitionScope;
  }
}
