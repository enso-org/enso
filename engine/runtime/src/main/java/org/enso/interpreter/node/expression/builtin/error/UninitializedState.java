package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "key")
public class UninitializedState extends AtomConstructor {
    public UninitializedState(ModuleScope definitionScope) {
        super("Uninitialized_State", definitionScope, true);
    }
}

