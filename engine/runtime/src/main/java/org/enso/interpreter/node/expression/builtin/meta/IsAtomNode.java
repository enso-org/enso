package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.atom.Atom;

@BuiltinMethod(
    type = "Meta",
    name = "is_atom",
    description = "Checks if the argument is an atom",
    autoRegister = false)
public class IsAtomNode extends Node {
  boolean execute(@AcceptsError Object value) {
    return value instanceof Atom;
  }
}
