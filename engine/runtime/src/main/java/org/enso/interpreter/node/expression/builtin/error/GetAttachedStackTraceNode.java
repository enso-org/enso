package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.runtime.GetStackTraceNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Panic",
    name = "primitive_get_attached_stack_trace",
    description = "Gets the stack trace attached to the throwable.")
public class GetAttachedStackTraceNode extends Node {
  @Child private InteropLibrary iop = InteropLibrary.getFactory().createDispatched(3);

  Object execute(@AcceptsError Object error) {
    if (iop.hasExceptionStackTrace(error)) {
      return GetStackTraceNode.stackTraceToArray(iop, error);
    }
    if (error instanceof Throwable) {
      return GetStackTraceNode.stackTraceToArray((Throwable) error);
    } else {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError("Throwable", error, "throwable"), this);
    }
  }
}
