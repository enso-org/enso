package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "Meta", name = "is_atom", description = "Checks if the argument is an atom")
public class IsAtomNode extends Node {
  boolean execute(Object self, @AcceptsError Object value) {
    return TypesGen.isAtom(value);
  }
}
