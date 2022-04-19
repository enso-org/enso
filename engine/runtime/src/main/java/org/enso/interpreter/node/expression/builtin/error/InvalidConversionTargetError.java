package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "target")
public class InvalidConversionTargetError extends AtomConstructor {
    public InvalidConversionTargetError(ModuleScope definitionScope) {
        super("Invalid_Conversion_Target_Error", definitionScope, true);
    }
}

