package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.array.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

public class Array {
  private final AtomConstructor array;

  public Array(Language language, ModuleScope scope) {
    array = new AtomConstructor("Array", scope).initializeFields();
    scope.registerConstructor(array);
    scope.registerMethod(array, "empty", EmptyMethodGen.makeFunction(language));
    scope.registerMethod(array, "new", NewMethodGen.makeFunction(language));
    scope.registerMethod(array, "length", LengthMethodGen.makeFunction(language));
    scope.registerMethod(array, "to_array", ToArrayMethodGen.makeFunction(language));
    scope.registerMethod(array, "at", GetAtMethodGen.makeFunction(language));
    scope.registerMethod(array, "set_at", SetAtMethodGen.makeFunction(language));
  }

  public AtomConstructor constructor() {
    return array;
  }
}
