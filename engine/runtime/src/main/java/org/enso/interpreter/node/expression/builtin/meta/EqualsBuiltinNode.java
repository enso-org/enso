package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeUtil;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.warning.AppendWarningNode;

@BuiltinMethod(
    type = "Any",
    name = "==",
    description =
        """
        Compares self with other object and returns True iff `self` is exactly the same as
        the other object, including all its transitively accessible properties or fields,
        False otherwise.

        Can handle arbitrary objects, including all foreign objects.

        Does not throw dataflow errors or panics.

        Note that this is different than `Meta.is_same_object`, which checks whether two
        references point to the same object on the heap. Moreover, `Meta.is_same_object`
        implies `Any.==` for all object with the exception of `Number.nan`.
        """)
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

  @CompilerDirectives.TruffleBoundary
  private static boolean confirmStop() {
    return new java.io.File("/tmp/stop").exists();
  }

  /**
   * Compares two objects for equality.
   *
   * @param frame the stack frame we are executing at
   * @param left the self object
   * @param right the other object
   * @return {@code true} if {@code self} and {@code that} seem equal
   */
  public Object execute(VirtualFrame frame, Object left, Object right) {
    if (left instanceof DataflowError e) {
      return e;
    }
    var f1 = left instanceof Double;
    var f2 = right instanceof Double;
    if ((f1 != f2) && confirmStop()) {
      breakpointHit(f1, f2);
    }
    var areEqual = node.execute(frame, left, right);
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

  @CompilerDirectives.TruffleBoundary
  private void breakpointHit(Object o1, Object o2) {
    System.err.println("EqualsNode for " + o1 + " and " + o2);
    System.err.println("         types " + o1.getClass() + " and " + o2.getClass());
    NodeUtil.printTree(System.err, this);
  }
}
