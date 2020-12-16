package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.mutable.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin array-related types and functions. */
public class Mutable {
  private final AtomConstructor array;
  private final AtomConstructor ref;

  /**
   * Creates a new instance of this class, registering all relevant bindings in the provided scope.
   *
   * @param language the current language instance.
   * @param scope the scope for builtin methods.
   */
  public Mutable(Language language, ModuleScope scope) {
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
    scope.registerMethod(array, "copy", CopyMethodGen.makeFunction(language));
    scope.registerMethod(array, "sort", SortMethodGen.makeFunction(language));

    ref = new AtomConstructor("Ref", scope).initializeFields();
    scope.registerConstructor(ref);
    scope.registerMethod(ref, "new", NewRefMethodGen.makeFunction(language));
    scope.registerMethod(ref, "get", GetRefMethodGen.makeFunction(language));
    scope.registerMethod(ref, "put", PutRefMethodGen.makeFunction(language));
  }

  /** @return the Array constructor. */
  public AtomConstructor array() {
    return array;
  }

  /** @return the Ref constructor. */
  public AtomConstructor ref() {
    return ref;
  }
}
