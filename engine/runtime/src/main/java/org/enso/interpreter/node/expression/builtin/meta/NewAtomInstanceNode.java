package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Meta",
    name = "new_atom",
    description = "Creates a new atom with given constructor and fields.")
public class NewAtomInstanceNode extends Node {
  Atom execute(Object _this, AtomConstructor constructor, Array fields) {
    return constructor.newInstance(fields.getItems());
  }
}
