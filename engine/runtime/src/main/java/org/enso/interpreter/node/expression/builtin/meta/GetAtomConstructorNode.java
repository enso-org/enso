package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Meta",
    name = "get_atom_constructor",
    description = "Gets the constructor of an atom.",
    autoRegister = false)
public abstract class GetAtomConstructorNode extends Node {
  abstract Object execute(Object atom);

  static GetAtomConstructorNode build() {
    return GetAtomConstructorNodeGen.create();
  }

  @Specialization
  Object doAtom(Atom atom) {
    var cons = atom.getConstructor();
    if (cons.getType().hasAllConstructorsPrivate()) {
      var ctx = EnsoContext.get(this);
      var err = ctx.getBuiltins().error().makePrivateAccessError(null, null, "constructor");
      return DataflowError.withDefaultTrace(err, this);
    }
    return cons;
  }

  @Specialization
  Object doType(Type type) {
    return type;
  }
}
