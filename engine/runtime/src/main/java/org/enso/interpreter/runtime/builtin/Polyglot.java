package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.interop.generic.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container class for all Polyglot-related stdlib builtins. */
public class Polyglot {

  private final AtomConstructor polyglot;

  /**
   * Creates and registers all polyglot-related functions and types.
   *
   * @param language the current language instance.
   * @param scope the builtin scope.
   */
  public Polyglot(Language language, ModuleScope scope) {
    this.polyglot = new AtomConstructor("Polyglot", scope).initializeFields();
    createPolyglot(language, scope);
  }

  private void createPolyglot(Language language, ModuleScope scope) {
    scope.registerConstructor(polyglot);
    scope.registerMethod(polyglot, "execute", ExecuteMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "invoke", InvokeMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "new", InstantiateMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_member", GetMemberMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_members", GetMembersMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_array_size", GetArraySizeMethodGen.makeFunction(language));
    scope.registerMethod(
        polyglot, "is_language_installed", IsLanguageInstalledMethodGen.makeFunction(language));
  }

  /** @return the atom constructor for polyglot */
  public AtomConstructor getPolyglot() {
    return polyglot;
  }
}
