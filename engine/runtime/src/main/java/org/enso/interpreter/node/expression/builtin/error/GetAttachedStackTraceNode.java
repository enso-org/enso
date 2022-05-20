package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.runtime.GetStackTraceNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Panic",
    name = "primitive_get_attached_stack_trace",
    description = "Gets the stack trace attached to the throwable.")
public class GetAttachedStackTraceNode extends Node {
  Array execute(Object _this, @AcceptsError Object error) {
    if (error instanceof Throwable) {
      return GetStackTraceNode.stackTraceToArray((Throwable) error);
    } else {
      Builtins builtins = Context.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError("Throwable", error, "throwable"), this);
    }
  }
}
