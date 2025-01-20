package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

@BuiltinMethod(
    type = "Meta",
    name = "type_of",
    description = "Returns the type of a value.",
    autoRegister = false)
final class TypeOfBuiltin extends Node {
  private @Child WarningsLibrary warnings = WarningsLibrary.getFactory().createDispatched(11);
  private @Child TypeOfNode typeOf = TypeOfNode.create();

  TypeOfBuiltin() {}

  public Object execute(@AcceptsError @AcceptsWarning Object value) {
    if (warnings.hasWarnings(value)) {
      try {
        value = warnings.removeWarnings(value);
      } catch (UnsupportedMessageException ex) {
        var ctx = EnsoContext.get(this);
        throw ctx.raiseAssertionPanic(this, null, ex);
      }
    }
    return typeOf.findTypeOrError(value);
  }
}
