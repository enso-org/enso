package org.enso.interpreter.node.expression.builtin.text;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinType
public class Text extends AtomConstructor {
    public Text(ModuleScope definitionScope) {
        super("Text", definitionScope, true);
    }
}
