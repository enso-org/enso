package org.enso.interpreter.node.expression.builtin.bool;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class False extends AtomConstructor {
    public False(ModuleScope definitionScope) {
        super("False", definitionScope, true);
    }
}
