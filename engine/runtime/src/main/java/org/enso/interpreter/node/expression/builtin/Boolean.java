package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

// Note that Boolean BuiltinType cannot be moved to `.expression.builtin.bool` package along with True and False
// because it currently breaks a lot of code generation for builtin methods.
// The name Boolean would clash with java.lang.Boolean.
// Before moving this definition to the `bool` package, as we should, one would have to address that problem first.
@BuiltinType
public class Boolean extends AtomConstructor {
    public Boolean(ModuleScope definitionScope) {
        super("Boolean", definitionScope, true);
    }
}
