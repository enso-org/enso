package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@BuiltinMethod(
    type = "Meta",
    name = "get_atom_constructor",
    description = "Gets the constructor of an atom.")
public class GetAtomConstructorNode extends Node {
  AtomConstructor execute(Atom atom) {
    return atom.getConstructor();
  }
}
