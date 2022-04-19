package org.enso.interpreter.node.expression.builtin.bool;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class True extends AtomConstructor {
    public True(ModuleScope definitionScope) {
        super("True", definitionScope, true);
    }
}
