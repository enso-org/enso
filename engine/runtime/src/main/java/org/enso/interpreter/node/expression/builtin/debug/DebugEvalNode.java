package org.enso.interpreter.node.expression.builtin.debug;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.expression.debug.EvalNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.state.Stateful;

/** Root node for the builtin Debug.eval function. */
@BuiltinMethod(
    type = "Debug",
    name = "eval",
    description = "Evaluates an expression passed as a Text argument, in the caller frame.",
    alwaysDirect = false)
public class DebugEvalNode extends Node {
  private @Child EvalNode evalNode = EvalNode.build();

  DebugEvalNode() {
    evalNode.markTail();
  }

  Stateful execute(
      CallerInfo callerInfo, @MonadicState Object state, Object _this, String expression) {
    return evalNode.execute(callerInfo, state, expression);
  }
}
