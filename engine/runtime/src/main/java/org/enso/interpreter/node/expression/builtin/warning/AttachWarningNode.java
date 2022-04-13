package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WithWarnings;

@BuiltinMethod(
    type = "Prim_Warning",
    name = "attach",
    description = "Attaches the given warning to the value.")
public abstract class AttachWarningNode extends Node {
  abstract WithWarnings execute(
      Object _this, @AcceptsWarning Object value, Object warning, Object origin);

  static AttachWarningNode build() {
    return AttachWarningNodeGen.create();
  }

  @Specialization
  WithWarnings doWarning(Object _this, WithWarnings value, Object warning, Object origin) {
    return value.prepend(new Warning(warning, origin, Context.get(this).clockTick()));
  }

  @Fallback
  WithWarnings doOther(Object _this, Object value, Object warning, Object origin) {
    return new WithWarnings(value, new Warning(warning, origin, Context.get(this).clockTick()));
  }
}
