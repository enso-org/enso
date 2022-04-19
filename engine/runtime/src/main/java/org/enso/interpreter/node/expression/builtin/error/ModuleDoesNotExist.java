package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType(params = "name")
public class ModuleDoesNotExist extends AtomConstructor {
    public ModuleDoesNotExist(ModuleScope definitionScope) {
        super("Module_Does_Not_Exist", definitionScope, true);
    }
}

