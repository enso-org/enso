package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;

@BuiltinMethod(
    type = "Meta",
    name = "new_atom",
    description = "Creates a new atom with given constructor and fields.",
    autoRegister = false)
public final class NewAtomInstanceNode extends Node {
  @Child private ArrayLikeCoerceToArrayNode coerce = ArrayLikeCoerceToArrayNode.build();
  @Child private AtomNewInstanceNode newNode = AtomNewInstanceNode.create();

  Atom execute(AtomConstructor constructor, Object fields) {
    Object[] args = coerce.execute(fields);
    return newNode.newInstance(constructor, args);
  }
}
