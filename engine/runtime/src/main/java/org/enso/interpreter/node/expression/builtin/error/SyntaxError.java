package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "message")
public class SyntaxError extends AtomConstructor {
    public SyntaxError(ModuleScope definitionScope) {
        super("Syntax_Error", definitionScope, true);
    }
}

