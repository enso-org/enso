package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "arguments")
public class UnsupportedArgumentTypes extends AtomConstructor {
    public UnsupportedArgumentTypes(ModuleScope definitionScope) {
        super("Unsupported_Argument_Types", definitionScope, true);
    }
}

