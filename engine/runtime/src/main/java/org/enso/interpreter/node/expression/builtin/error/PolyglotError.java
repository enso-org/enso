package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "cause")
public class PolyglotError extends AtomConstructor {
    public PolyglotError(ModuleScope definitionScope) {
        super("Polyglot_Error", definitionScope, true);
    }
}

