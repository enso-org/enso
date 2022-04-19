package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "expected,actual,name")
public class TypeError extends AtomConstructor {
    public TypeError(ModuleScope definitionScope) {
        super("Type_Error", definitionScope, true);
    }
}

