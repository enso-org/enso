package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "get_constructor_type",
    description = "Gets type the constructor belongs to.",
    autoRegister = false)
public abstract class GetConstructorTypeNode extends Node {
  static GetConstructorTypeNode build() {
    return GetConstructorTypeNodeGen.create();
  }

  abstract Object execute(Object atom_constructor);

  @Specialization
  Type doConstructor(AtomConstructor cons) {
    return cons.getType();
  }

  @Fallback
  Object doType(Object type) {
    var err = new PanicException(Text.create("Argument is not a Constructor"), this);
    return DataflowError.withTrace(type, err);
  }
}
