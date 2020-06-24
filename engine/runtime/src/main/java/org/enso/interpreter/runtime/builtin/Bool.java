package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.bool.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container class for all Boolean-related stdlib builtins. */
public class Bool {
  private final AtomConstructor tru;
  private final AtomConstructor fls;
  private final AtomConstructor bool;

  /**
   * Creates and registers all the boolean constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors and methods in.
   */
  public Bool(Language language, ModuleScope scope) {
    bool = new AtomConstructor("Boolean", scope).initializeFields();
    scope.registerConstructor(bool);
    scope.registerMethod(bool, "if_then_else", IfThenElseNode.makeFunction(language));
    scope.registerMethod(bool, "to_text", ToTextNode.makeFunction(language));
    scope.registerMethod(bool, "&&", AndNode.makeFunction(language));
    scope.registerMethod(bool, "||", OrNode.makeFunction(language));
    scope.registerMethod(bool, "not", NotNode.makeFunction(language));
    tru = new AtomConstructor("True", scope).initializeFields();
    scope.registerConstructor(tru);
    fls = new AtomConstructor("False", scope).initializeFields();
    scope.registerConstructor(fls);
  }

  /** @return the atom constructor for {@code True}. */
  public AtomConstructor getTrue() {
    return tru;
  }

  /** @return the atom constructor for {@code False}. */
  public AtomConstructor getFalse() {
    return fls;
  }

  /** @return the atom constructor for {@code Boolean}. */
  public AtomConstructor getBool() {
    return bool;
  }
}
