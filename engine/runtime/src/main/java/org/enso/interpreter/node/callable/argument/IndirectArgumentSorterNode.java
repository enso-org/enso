package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo.ArgumentMapping;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.State;

/**
 * This class handles the case where a mapping for reordering arguments to a given callable has
 * already been computed.
 *
 * <p>It is the slow-path, uncached version of the operation.
 */
@NodeInfo(description = "Reorders the arguments and executes them.")
@GenerateUncached
public abstract class IndirectArgumentSorterNode extends Node {

  /**
   * Creates an instance of this node.
   *
   * @return a new instance of this node.
   */
  public static IndirectArgumentSorterNode build() {
    return IndirectArgumentSorterNodeGen.create();
  }

  @ExplodeLoop
  private void executeArguments(
      VirtualFrame frame,
      ArgumentMapping mapping,
      Object[] arguments,
      State state,
      ThunkExecutorNode thunkExecutorNode) {
    for (int i = 0; i < mapping.getArgumentShouldExecute().length; i++) {
      if (mapping.getArgumentShouldExecute()[i]) {
        arguments[i] =
            thunkExecutorNode.executeThunk(
                frame, arguments[i], state, BaseNode.TailStatus.NOT_TAIL);
      }
    }
  }

  /**
   * Reorders and executes the provided arguments in a way suitable for the called function.
   *
   * @param frame current frame
   * @param preApplicationSchema the function schema before applying the arguments
   * @param mapping the pre-computed argument mapping for the function
   * @param argumentsExecutionMode whether arguments should be executed or not
   * @param function the function this node is reordering arguments for
   * @param state the state to pass to the function
   * @param arguments the arguments to reorder
   * @return the provided {@code arguments} in the order expected by the cached {@link Function}
   */
  public abstract ArgumentSorterNode.MappedArguments execute(
      VirtualFrame frame,
      FunctionSchema preApplicationSchema,
      ArgumentMapping mapping,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      Function function,
      State state,
      Object[] arguments);

  @Specialization
  ArgumentSorterNode.MappedArguments doExecute(
      VirtualFrame frame,
      FunctionSchema preApplicationSchema,
      ArgumentMapping mapping,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      Function function,
      State state,
      Object[] arguments,
      @Cached ThunkExecutorNode thunkExecutorNode) {
    FunctionSchema postApplicationSchema = mapping.getPostApplicationSchema();
    if (argumentsExecutionMode.shouldExecute()) {
      executeArguments(frame, mapping, arguments, state, thunkExecutorNode);
    }
    Object[] mappedAppliedArguments =
        ArgumentSorterNode.prepareArguments(
            preApplicationSchema, postApplicationSchema, mapping, function, arguments);
    Object[] oversaturatedArguments = null;
    if (postApplicationSchema.hasOversaturatedArgs()) {
      oversaturatedArguments =
          generateOversaturatedArguments(
              preApplicationSchema, postApplicationSchema, mapping, function, arguments);
    }
    return new ArgumentSorterNode.MappedArguments(mappedAppliedArguments, oversaturatedArguments);
  }

  private Object[] generateOversaturatedArguments(
      FunctionSchema preApplicationSchema,
      FunctionSchema postApplicationSchema,
      ArgumentMapping mapping,
      Function function,
      Object[] arguments) {
    Object[] oversaturatedArguments =
        new Object[postApplicationSchema.getOversaturatedArguments().length];

    System.arraycopy(
        function.getOversaturatedArguments(),
        0,
        oversaturatedArguments,
        0,
        preApplicationSchema.getOversaturatedArguments().length);

    mapping.obtainOversaturatedArguments(
        arguments, oversaturatedArguments, preApplicationSchema.getOversaturatedArguments().length);

    return oversaturatedArguments;
  }
}
