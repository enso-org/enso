package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.WithWarnings;

@BuiltinMethod(
    type = "Error",
    name = "throw",
    description = "Returns a new value error with given payload.")
public abstract class GetWarningsNode extends Node {
  abstract Array execute(Object _this, @AcceptsWarning Object value);

  static GetWarningsNode build() {
    return GetWarningsNodeGen.create();
  }

  @Specialization
  Array doWarning(Object _this, WithWarnings value) {
    return new Array(value.getWarningsArray());
  }

  @Fallback
  Array doOther(Object _this, Object value) {
    return new Array();
  }
}
