package org.enso.interpreter.runtime;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.PrintNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container class for static predefined atoms, methods, and their containing scope. */
public class Builtins {
  private final ModuleScope scope;
  private final AtomConstructor unit;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param language the current {@link Language} instance
   */
  public Builtins(Language language) {
    scope = new ModuleScope();
    unit = new AtomConstructor("Unit", scope).initializeFields();

    AtomConstructor nil = new AtomConstructor("Nil", scope).initializeFields();
    AtomConstructor cons =
        new AtomConstructor("Cons", scope)
            .initializeFields(
                new ArgumentDefinition(0, "head", false), new ArgumentDefinition(1, "rest", false));
    AtomConstructor io = new AtomConstructor("IO", scope).initializeFields();

    scope.registerConstructor(cons);
    scope.registerConstructor(nil);
    scope.registerConstructor(unit);
    scope.registerConstructor(io);

    scope.registerMethod(io, "println", PrintNode.toFunction(language));
  }

  /**
   * Returns the {@code Unit} atom constructor.
   *
   * @return the {@code Unit} atom constructor
   */
  public AtomConstructor unit() {
    return unit;
  }

  /**
   * Returns the builtin module scope.
   *
   * @return the builtin module scope
   */
  public ModuleScope getScope() {
    return scope;
  }
}
