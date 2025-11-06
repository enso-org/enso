package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.warning.AppendWarningNode;

@BuiltinMethod(
    type = "Any_Helpers",
    name = "any_equals",
    autoRegister=false
)
public final class EqualsBuiltinNode extends Node {
  @Child private EqualsNode node;
  @Child private AppendWarningNode append;

  private EqualsBuiltinNode(EqualsNode node) {
    this.node = node;
  }

  @NeverDefault
  static EqualsBuiltinNode build() {
    return new EqualsBuiltinNode(EqualsNode.build());
  }

  /**
   * Compares two objects for equality.
   *
   * @param frame the stack frame we are executing at
   * @param obj the self object
   * @param other the other object
   * @return {@code true} if {@code self} and {@code that} seem equal
   */
  public Object execute(VirtualFrame frame, Object obj, Object other) {
    if (obj instanceof DataflowError e) {
      return e;
    }
    var areEqual = node.execute(frame, obj, other);
    if (areEqual.getWarnings() != null) {
      if (append == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        append = insert(AppendWarningNode.build());
      }
      return append.executeAppend(frame, areEqual.isTrue(), areEqual.getWarnings());
    } else {
      return areEqual.isTrue();
    }
  }
}
