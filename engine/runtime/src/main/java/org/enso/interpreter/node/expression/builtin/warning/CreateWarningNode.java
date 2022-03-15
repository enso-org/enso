package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.Warning;

@BuiltinMethod(
    type = "Prim_Warning",
    name = "create",
    description = "Creates a new instance of the primitive warning value.")
public class CreateWarningNode extends Node {
  Warning execute(Object _this, Object payload, Object origin) {
    return new Warning(payload, origin, Context.get(this).clockTick());
  }
}
