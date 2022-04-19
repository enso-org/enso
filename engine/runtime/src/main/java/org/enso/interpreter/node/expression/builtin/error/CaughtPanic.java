package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params="payload,internal_original_exception")
public class CaughtPanic extends AtomConstructor {
    public CaughtPanic(ModuleScope definitionScope) {
        super("Caught_Panic", definitionScope, true);
    }
}