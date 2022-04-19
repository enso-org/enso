package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "expected_min,expected_max,actual")
public class ArityError extends AtomConstructor {
    public ArityError(ModuleScope definitionScope) {
        super("Arity_Error", definitionScope, true);
    }
}

