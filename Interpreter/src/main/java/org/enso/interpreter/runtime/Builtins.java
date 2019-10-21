package org.enso.interpreter.runtime;

import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container class for static predefined atoms and their containing scope. */
public class Builtins {
  public static final ModuleScope BUILTIN_SCOPE = new ModuleScope();
  public static final AtomConstructor UNIT =
      new AtomConstructor("Unit", BUILTIN_SCOPE).initializeFields();
  public static final AtomConstructor NIL =
      new AtomConstructor("Nil", BUILTIN_SCOPE).initializeFields();
  public static final AtomConstructor CONS =
      new AtomConstructor("Cons", BUILTIN_SCOPE)
          .initializeFields(
              new ArgumentDefinition(0, "head", false), new ArgumentDefinition(1, "rest", false));

  static {
    BUILTIN_SCOPE.registerConstructor(Builtins.CONS);
    BUILTIN_SCOPE.registerConstructor(Builtins.NIL);
    BUILTIN_SCOPE.registerConstructor(Builtins.UNIT);
  }
}
