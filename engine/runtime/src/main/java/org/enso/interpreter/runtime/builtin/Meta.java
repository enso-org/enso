package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.meta.*;
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
        meta, "is_unresolved_symbol", IsUnresolvedSymbolMethodGen.makeFunction(language));
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

    scope.registerMethod(meta, "is_constructor", IsAtomConstructorMethodGen.makeFunction(language));
    scope.registerMethod(
        meta, "get_constructor_name", GetConstructorNameMethodGen.makeFunction(language));
    scope.registerMethod(
        meta, "get_constructor_fields", GetConstructorFieldNamesMethodGen.makeFunction(language));
    scope.registerMethod(meta, "new_atom", NewAtomInstanceMethodGen.makeFunction(language));

    scope.registerMethod(meta, "is_atom", IsAtomMethodGen.makeFunction(language));
    scope.registerMethod(meta, "get_atom_fields", GetAtomFieldsMethodGen.makeFunction(language));
    scope.registerMethod(
        meta, "get_atom_constructor", GetAtomConstructorMethodGen.makeFunction(language));

    scope.registerMethod(meta, "is_error", IsErrorMethodGen.makeFunction(language));

    scope.registerMethod(meta, "is_polyglot", IsPolyglotMethodGen.makeFunction(language));
    scope.registerMethod(
        meta, "get_polyglot_language", GetPolyglotLanguageMethodGen.makeFunction(language));

    scope.registerMethod(meta, "is_same_object", IsSameObjectMethodGen.makeFunction(language));
  }
}
