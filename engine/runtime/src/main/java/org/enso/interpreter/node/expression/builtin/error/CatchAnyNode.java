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

@BuiltinMethod(
    type = "Any_Helpers",
    name = "catch_primitive",
    autoRegister = false
)
public abstract class CatchAnyNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;

  CatchAnyNode() {
    this.invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
    this.invokeCallableNode.setTailStatus(BaseNode.TailStatus.TAIL_DIRECT);
  }
  public static CatchErrorNode build() {
    return CatchErrorNodeGen.create();
  }

  abstract Object execute(Object obj, Object handler);

  @Specialization
  Object doDataflowError(VirtualFrame frame, DataflowError self, Object handler) {
    return invokeCallableNode.execute(
        handler, frame, EnsoContext.get(this).currentState(), new Object[] {self.getPayload()});
  }

  @Fallback
  Object doOther(VirtualFrame frame, Object self, Object handler) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var typeErr = builtins.error().makeTypeError("Dataflow_Error", self, "self");
    throw new PanicException(typeErr, this);
  }
}
