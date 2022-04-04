package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.mutable.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin array-related types and functions. */
public class Mutable {
  private final AtomConstructor array;
  private final AtomConstructor ref;
  private final Builtins builtins;

  /**
   * Creates a new instance of this class, registering all relevant bindings in the provided scope.
   *
   * @param language the current language instance.
   * @param scope the scope for builtin methods.
   */
  public Mutable(Builtins builtins, Language language, ModuleScope scope) {
    array = null;
    this.builtins = builtins;

    ref = new AtomConstructor("Ref", scope).initializeFields();
    scope.registerConstructor(ref);
    scope.registerMethod(ref, "new", NewRefMethodGen.makeFunction(language));
    scope.registerMethod(ref, "get", GetRefMethodGen.makeFunction(language));
    scope.registerMethod(ref, "put", PutRefMethodGen.makeFunction(language));
  }

  /** @return the Array constructor. */
  public AtomConstructor array() {
    return builtins.getBuiltinType("Array");
  }

  /** @return the Ref constructor. */
  public AtomConstructor ref() {
    return ref;
  }
}
