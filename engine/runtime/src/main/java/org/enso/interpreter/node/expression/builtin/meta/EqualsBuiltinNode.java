package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
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

  /*
    @NeverDefault
    public static EqualsBuiltinNode create() {
      return new EqualsBuiltinNode(EqualsNode.build());
    }

    @NeverDefault
    public static EqualsBuiltinNode getUncached() {
      return UNCACHED;
    }
  */
  /**
   * Compares two objects for equality. If the {@link EqualsSimpleNode simple check} fails, it tries
   * to convert first argument to the second one and compare again.
   *
   * @param frame the stack frame we are executing at
   * @param self the self object
   * @param other the other object
   * @return {@code true} if {@code self} and {@code that} seem equal
   */
  public Object execute(VirtualFrame frame, Object self, Object other) {
    var areEqual = node.execute(frame, self, other);
    if (areEqual.warnings() != null) {
      if (append == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        append = insert(AppendWarningNode.build());
      }
      return append.executeAppend(frame, areEqual.equals(), areEqual.warnings());
    } else {
      return areEqual.equals();
    }
  }
}
