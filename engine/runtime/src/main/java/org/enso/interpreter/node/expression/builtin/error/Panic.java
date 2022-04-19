package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Panic extends AtomConstructor {
    public Panic(ModuleScope definitionScope) {
        super("Panic", definitionScope, true);
    }
}