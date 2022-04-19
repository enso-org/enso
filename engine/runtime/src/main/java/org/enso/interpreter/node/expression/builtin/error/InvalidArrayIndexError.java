package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "array,index")
public class InvalidArrayIndexError extends AtomConstructor {
    public InvalidArrayIndexError(ModuleScope definitionScope) {
        super("Invalid_Array_Index_Error", definitionScope, true);
    }
}

