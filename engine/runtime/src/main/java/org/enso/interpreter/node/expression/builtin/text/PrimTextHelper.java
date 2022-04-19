package org.enso.interpreter.node.expression.builtin.text;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class PrimTextHelper extends AtomConstructor {
    public PrimTextHelper(ModuleScope definitionScope) {
        super("Prim_Text_Helper", definitionScope, true);
    }
}