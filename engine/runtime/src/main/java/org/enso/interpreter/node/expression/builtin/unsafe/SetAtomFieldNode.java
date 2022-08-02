package org.enso.interpreter.node.expression.builtin.unsafe;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;

@BuiltinMethod(
    type = "Unsafe",
    name = "set_atom_field",
    description = "Unsafely, in place, sets the value of an atom field by index.")
public class SetAtomFieldNode extends Node {
  Atom execute(Atom atom, long index, Object value) {
    atom.getFields()[(int) index] = value;
    return atom;
  }
}
