package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.Arrays;

/**
 * This node is responsible for organising callable calls so that they are ready to be made.
 *
 * <p>It handles computing the values of the arguments to the callable, and also the sorting of
 * those arguments into the correct positional order for the callable being called.
 */
@NodeInfo(shortName = "@", description = "Executes function")
public class ApplicationNode extends ExpressionNode {

  @CompilationFinal(dimensions = 1)
  private RootCallTarget[] argExpressions;

  @Child private InvokeCallableNode invokeCallableNode;
  @Child private ExpressionNode callable;

  /**
   * Creates a new node for performing callable invocation.
   *
   * @param callArguments information on the arguments being passed to the {@link Function}
   */
  public ApplicationNode(
      ExpressionNode callable,
      CallArgument[] callArguments,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode) {
    this.argExpressions =
        Arrays.stream(callArguments)
            .map(CallArgument::getExpression)
            .toArray(RootCallTarget[]::new);

    CallArgumentInfo[] argSchema =
        Arrays.stream(callArguments).map(CallArgumentInfo::new).toArray(CallArgumentInfo[]::new);

    this.callable = callable;
    this.invokeCallableNode =
        InvokeCallableNodeGen.create(
            argSchema, defaultsExecutionMode, InvokeCallableNode.ArgumentsExecutionMode.EXECUTE);
  }

  /**
   * Marks whether the {@code argumentSorter} child is tail–recursive.
   *
   * @param isTail whether or not the node is tail-recursive.
   */
  @Override
  public void setTail(boolean isTail) {
    super.setTail(isTail);
    invokeCallableNode.setTail(isTail);
  }

  /**
   * Evaluates the arguments being passed to the function.
   *
   * @param frame the stack frame in which to execute
   * @return the results of evaluating the function arguments
   */
  @ExplodeLoop
  public Object[] evaluateArguments(VirtualFrame frame) {
    Object[] computedArguments = new Object[this.argExpressions.length];
    MaterializedFrame scope = frame.materialize();
    for (int i = 0; i < this.argExpressions.length; ++i) {
      computedArguments[i] = new Thunk(this.argExpressions[i], scope);
    }
    return computedArguments;
  }

  /**
   * Executes the application of arguments to a callable.
   *
   * @param frame the stack frame for execution
   * @return the value that results from evaluating the callable
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    Stateful result = this.invokeCallableNode.execute(
        this.callable.executeGeneric(frame), state, evaluateArguments(frame));
    frame.setObject(getStateFrameSlot(), result.getState());
    return result.getValue();
  }
}
