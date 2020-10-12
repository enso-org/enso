package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(
    type = "Meta",
    name = "get_unresolved_symbol_scope",
    description = "Gets the scope of an unresolved symbol")
public class GetAtomConstructorNode extends Node {
  AtomConstructor execute(Object _this, Atom atom) {
    return atom.getConstructor();
  }
}
