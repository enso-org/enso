package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.Builtin;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Array extends Builtin {
    public Array(ModuleScope definitionScope) {
        super("Array", definitionScope);
    }
}

