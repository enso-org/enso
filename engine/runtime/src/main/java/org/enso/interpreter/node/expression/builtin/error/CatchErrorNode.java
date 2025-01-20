package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Error",
    name = "catch_primitive",
    description =
        "If called on an error, executes the provided handler on the error's payload. Otherwise"
            + " acts as identity.")
public abstract class CatchErrorNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;

  public abstract Object execute(VirtualFrame frame, State state, Object self, Object handler);

  public static CatchErrorNode build() {
    return CatchErrorNodeGen.create();
  }

  CatchErrorNode() {
    this.invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
    this.invokeCallableNode.setTailStatus(BaseNode.TailStatus.TAIL_DIRECT);
  }

  @Specialization
  Object doDataflowError(VirtualFrame frame, State state, DataflowError self, Object handler) {
    return invokeCallableNode.execute(handler, frame, state, new Object[] {self.getPayload()});
  }

  @Fallback
  Object doOther(VirtualFrame frame, State state, Object self, Object handler) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var typeErr = builtins.error().makeTypeError("Dataflow_Error", self, "self");
    throw new PanicException(typeErr, this);
  }
}
