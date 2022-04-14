package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Any extends AtomConstructor {
    public Any(ModuleScope definitionScope) {
        super("Any", definitionScope, true);
    }
}
