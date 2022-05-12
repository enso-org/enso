package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WithWarnings;

@BuiltinMethod(
    type = "Prim_Warning",
    name = "set",
    description = "Attaches the given warning to the value.")
public abstract class SetWarningsNode extends Node {
  abstract Object execute(Object _this, @AcceptsWarning Object value, Array warnings);

  static SetWarningsNode build() {
    return SetWarningsNodeGen.create();
  }

  @Specialization
  Object doWarning(Object _this, WithWarnings value, Array warnings) {
    if (warnings.length() == 0) {
      return value.getValue();
    }
    Warning[] warningsCast = new Warning[(int) warnings.length()];
    System.arraycopy(warnings.getItems(), 0, warningsCast, 0, warningsCast.length);
    return new WithWarnings(value.getValue(), warningsCast);
  }

  @Fallback
  Object doOther(Object _this, Object value, Array warnings) {
    if (warnings.length() == 0) {
      return value;
    }
    Warning[] warningsCast = new Warning[(int) warnings.length()];
    System.arraycopy(warnings.getItems(), 0, warningsCast, 0, warningsCast.length);
    return new WithWarnings(value, warningsCast);
  }
}
