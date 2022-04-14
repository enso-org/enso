package org.enso.interpreter.node.expression.builtin.mutable;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Ref extends AtomConstructor {
    public Ref(ModuleScope definitionScope) {
        super("Ref", definitionScope, true);
    }
}
