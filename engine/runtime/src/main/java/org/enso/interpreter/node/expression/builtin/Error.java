package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Error extends AtomConstructor {
    public Error(ModuleScope definitionScope) {
        super("Error", definitionScope, true);
    }
}