package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Runtime",
    name = "primitive_get_stack_trace",
    description = "Gets the current execution stacktrace.")
public class GetStackTraceNode extends Node {
  Array execute(Object _this) {
    var exception = new PanicException(null, this);
    TruffleStackTrace.fillIn(exception);
    var elements = TruffleStackTrace.getStackTrace(exception);
    var ret = new Array(elements.size());
    for (int i = 0; i < elements.size(); i++) {
      var element = elements.get(i);
      ret.getItems()[i] = element.getGuestObject();
    }
    return ret;
  }
}
