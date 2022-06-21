package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(
    type = "Meta",
    name = "get_atom_fields",
    description = "Gets the fields of an unresolved atom.")
public class GetAtomFieldsNode extends Node {
  Array execute(Object _this, Atom atom) {
    return new Array(atom.getFields());
  }
}
