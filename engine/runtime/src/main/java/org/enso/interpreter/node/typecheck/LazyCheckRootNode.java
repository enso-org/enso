package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;

final class LazyCheckRootNode extends RootNode {

  @Child private ThunkExecutorNode evalThunk;
  @Child private TypeCheckValueNode check;
  static final FunctionSchema SCHEMA =
      FunctionSchema.newBuilder()
          .argumentDefinitions(
              new ArgumentDefinition(
                  0, "delegate", null, null, ArgumentDefinition.ExecutionMode.EXECUTE))
          .hasPreapplied(true)
          .build();

  LazyCheckRootNode(TruffleLanguage<?> language, TypeCheckValueNode check) {
    super(language);
    this.check = check;
    this.evalThunk = ThunkExecutorNode.build();
  }

  Function wrapThunk(Function thunk) {
    return new Function(getCallTarget(), thunk.getScope(), SCHEMA, new Object[] {thunk}, null);
  }

  @Override
  public Object execute(VirtualFrame frame) {
    var state = Function.ArgumentsHelper.getState(frame.getArguments());
    var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
    assert args.length == 1;
    assert args[0] instanceof Function fn && fn.isThunk();
    var raw = evalThunk.executeThunk(frame, args[0], state, BaseNode.TailStatus.NOT_TAIL);
    var result = check.handleCheckOrConversion(frame, raw, null);
    return result;
  }
}
