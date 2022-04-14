package org.enso.interpreter.node.expression.builtin.mutable;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Array extends AtomConstructor {
    public Array(ModuleScope definitionScope) {
        super("Array", definitionScope, true);
    }
}

