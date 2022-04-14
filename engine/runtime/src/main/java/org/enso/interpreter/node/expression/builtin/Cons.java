package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params="head,tail")
public class Cons extends AtomConstructor {
    public Cons(ModuleScope definitionScope) {
        super("Cons", definitionScope, true);
    }
}

