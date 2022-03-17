package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Error",
    name = "primitive_get_stack_trace",
    description = "Gets the stack trace of the error's origin.")
public class GetStackTraceNode extends Node {
  Array execute(DataflowError _this) {
    var elements = TruffleStackTrace.getStackTrace(_this);
    var ret = new Array(elements.size());
    for (int i = 0; i < elements.size(); i++) {
      var element = elements.get(i);
      ret.getItems()[i] = element.getGuestObject();
    }
    return ret;
  }
}
