package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Nil extends AtomConstructor {
    public Nil(ModuleScope definitionScope) {
        super("Nil", definitionScope, true);
    }
}
