package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(name = "get", description = "Gets the value stored in the reference", type = "Ref")
final class RefGetNode extends BaseNode {
  @Child private ThunkExecutorNode thunkNode;

  @NeverDefault
  static RefGetNode build() {
    return new RefGetNode();
  }

  /**
   * @return the current value of the reference. Evaluates it if it is not yet evaluated.
   */
  final Object execute(VirtualFrame frame, Ref ref) {
    if (ref.needsEval()) {
      if (thunkNode == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        thunkNode = ThunkExecutorNode.build();
      }
      var ctx = EnsoContext.get(this);
      var state = ctx.currentState();
      var newValue =
          thunkNode.executeThunk(
              frame.materialize(), ref.value(), state, BaseNode.TailStatus.NOT_TAIL);
      Ref.putValue(ref, newValue);
    }
    return ref.value();
  }
}
