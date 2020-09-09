package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.array.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin array-related types and functions. */
public class Array {
  private final AtomConstructor array;

  /**
   * Creates a new instance of this class, registering all relevant bindings in the provided scope.
   *
   * @param language the current language instance.
   * @param scope the scope for builtin methods.
   */
  public Array(Language language, ModuleScope scope) {
    array = new AtomConstructor("Array", scope).initializeFields();
    scope.registerConstructor(array);
    scope.registerMethod(array, "empty", EmptyMethodGen.makeFunction(language));
    scope.registerMethod(array, "new", NewMethodGen.makeFunction(language));
    scope.registerMethod(array, "new_1", New1MethodGen.makeFunction(language));
    scope.registerMethod(array, "new_2", New2MethodGen.makeFunction(language));
    scope.registerMethod(array, "new_3", New3MethodGen.makeFunction(language));
    scope.registerMethod(array, "new_4", New4MethodGen.makeFunction(language));
    scope.registerMethod(array, "length", LengthMethodGen.makeFunction(language));
    scope.registerMethod(array, "to_array", ToArrayMethodGen.makeFunction(language));
    scope.registerMethod(array, "at", GetAtMethodGen.makeFunction(language));
    scope.registerMethod(array, "set_at", SetAtMethodGen.makeFunction(language));
  }

  /** @return the Array constructor. */
  public AtomConstructor constructor() {
    return array;
  }
}
