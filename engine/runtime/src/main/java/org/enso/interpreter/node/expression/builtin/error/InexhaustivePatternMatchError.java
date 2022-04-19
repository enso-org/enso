package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "scrutinee")
public class InexhaustivePatternMatchError extends AtomConstructor {
    public InexhaustivePatternMatchError(ModuleScope definitionScope) {
        super("Inexhaustive_Pattern_Match_Error", definitionScope, true);
    }
}

