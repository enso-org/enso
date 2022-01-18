package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.WithWarnings;

@BuiltinMethod(
    type = "Error",
    name = "throw",
    description = "Returns a new value error with given payload.")
public abstract class AttachWarningNode extends Node {
  abstract WithWarnings execute(Object _this, @AcceptsWarning Object value, Object warning);

  static AttachWarningNode build() {
    return AttachWarningNodeGen.create();
  }

  @Specialization
  WithWarnings doWarning(Object _this, WithWarnings value, Object warning) {
    return value.prepend(warning);
  }

  @Fallback
  WithWarnings doOther(Object _this, Object value, Object warning) {
    return new WithWarnings(value, warning);
  }
}
