package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class ModuleNotInPackageError extends AtomConstructor {
    public ModuleNotInPackageError(ModuleScope definitionScope) {
        super("Module_Not_In_Package_Error", definitionScope, true);
    }
}

