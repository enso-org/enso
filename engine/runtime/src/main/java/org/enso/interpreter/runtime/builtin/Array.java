package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.array.EmptyMethodGen;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

public class Array {
  private final AtomConstructor array;

  public Array(Language language, ModuleScope scope) {
    array = new AtomConstructor("Array", scope).initializeFields();
    scope.registerConstructor(array);
    scope.registerMethod(array, "empty", EmptyMethodGen.makeFunction(language));
  }
}
