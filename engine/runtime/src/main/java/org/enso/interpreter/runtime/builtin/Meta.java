package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.meta.CreateUnresolvedSymbolMethodGen;
import org.enso.interpreter.node.expression.builtin.meta.GetUnresolvedSymbolNameMethodGen;
import org.enso.interpreter.node.expression.builtin.meta.GetUnresolvedSymbolScopeMethodGen;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin Meta programming capabilities */
public class Meta {

  /**
   * Creates and registers the relevant constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors in.
   */
  public Meta(Language language, ModuleScope scope) {
    AtomConstructor meta = new AtomConstructor("Meta", scope).initializeFields();
    scope.registerConstructor(meta);

    scope.registerMethod(
        meta,
        "get_unresolved_symbol_name",
        GetUnresolvedSymbolNameMethodGen.makeFunction(language));
    scope.registerMethod(
        meta,
        "get_unresolved_symbol_scope",
        GetUnresolvedSymbolScopeMethodGen.makeFunction(language));
    scope.registerMethod(
        meta, "create_unresolved_symbol", CreateUnresolvedSymbolMethodGen.makeFunction(language));
  }
}
