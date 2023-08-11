package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;

@BuiltinMethod(
    type = "Meta",
    name = "new_atom",
    description = "Creates a new atom with given constructor and fields.",
    autoRegister = false)
public abstract class NewAtomInstanceNode extends Node {

  static NewAtomInstanceNode build() {
    return NewAtomInstanceNodeGen.create();
  }

  abstract Atom execute(AtomConstructor constructor, Object fields);

  @Specialization
  Atom doExecute(
      AtomConstructor constructor, Object fields, @Cached ArrayLikeCoerceToArrayNode coerce) {
    return constructor.newInstance(coerce.execute(fields));
  }
}
