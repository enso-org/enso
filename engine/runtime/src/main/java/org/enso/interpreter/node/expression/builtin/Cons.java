package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.Builtin;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params="head,tail")
public class Cons extends Builtin {
    public Cons(ModuleScope definitionScope) {
        super("Cons", definitionScope);
    }
}

