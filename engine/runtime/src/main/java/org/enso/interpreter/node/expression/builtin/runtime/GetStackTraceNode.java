package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Runtime",
    name = "primitive_get_stack_trace",
    description = "Gets the current execution stacktrace.",
    autoRegister = false)
public class GetStackTraceNode extends Node {
  EnsoObject execute(VirtualFrame requestOwnStackFrame) {
    var exception = new PanicException("Stacktrace", this);
    TruffleStackTrace.fillIn(exception);
    return stackTraceToArray(exception);
  }

  @CompilerDirectives.TruffleBoundary
  public static EnsoObject stackTraceToArray(Throwable exception) {
    var elements = TruffleStackTrace.getStackTrace(exception);
    if (elements == null) {
      return ArrayLikeHelpers.empty();
    }
    int count = 0;
    for (int i = 0; i < elements.size(); i++) {
      var element = elements.get(i);
      if (element.getTarget().getRootNode().isInternal()) {
        continue;
      }
      count++;
    }
    var arr = new Object[count];
    for (int i = 0, at = 0; i < elements.size(); i++) {
      var element = elements.get(i);
      if (element.getTarget().getRootNode().isInternal()) {
        continue;
      }
      arr[at++] = element.getGuestObject();
    }
    return ArrayLikeHelpers.wrapObjectsWithCheckAt(arr);
  }
}
