package org.enso.interpreter.node.expression.builtin.unsafe;

import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;

@BuiltinMethod(
    type = "Unsafe",
    name = "create_thread",
    description =
        "Create a new thread evaluating the given expression. Currently only exposed for testing and should not be used for _any_ use cases.")
@ReportPolymorphism
public class CreateThreadNode extends Node {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  public Object execute(@MonadicState Object state, Object _this, @Suspend Object action) {
    Context context = lookupContextReference(Language.class).get();
    context
        .getEnvironment()
        .createThread(
            () -> thunkExecutorNode.executeThunk(action, state, BaseNode.TailStatus.NOT_TAIL)).start();
    return context.getBuiltins().nothing().newInstance();
  }
}
