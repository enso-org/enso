package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(
    type = "Runtime",
    name = "assertions_enabled",
    description = "Returns True iff assertions are enabled")
public class IsAssertionEnabledNode extends Node {
  public static IsAssertionEnabledNode build() {
    return new IsAssertionEnabledNode();
  }

  public boolean execute(VirtualFrame frame) {
    return EnsoContext.get(this).isAssertionsEnabled();
  }
}
