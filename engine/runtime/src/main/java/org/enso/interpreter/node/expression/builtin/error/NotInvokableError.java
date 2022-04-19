package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "target")
public class NotInvokableError extends AtomConstructor {
    public NotInvokableError(ModuleScope definitionScope) {
        super("Not_Invokable_Error", definitionScope, true);
    }
}

