package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.callable.CaptureCallerInfoNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.argument.ArgumentSorterNode;
import org.enso.interpreter.node.callable.argument.IndirectArgumentSorterNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * Executes a function with given arguments.
 *
 * <p>This is a slow-path node, for the uncached flow.
 */
@ImportStatic({CallArgumentInfo.ArgumentMappingBuilder.class})
@GenerateUncached
public abstract class IndirectInvokeFunctionNode extends Node {

  /**
   * Executes the {@link IndirectInvokeFunctionNode} to apply the function to given arguments.
   *
   * @param callable the function to call
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments being passed to {@code function}
   * @param schema the names and ordering of arguments for this call site
   * @param defaultsExecutionMode whether defaults are suspended for this call.
   * @param argumentsExecutionMode whether arguments are preexecuted for this call.
   * @param isTail is the call happening in a tail position.
   * @return the result of executing the {@code function} with reordered {@code arguments}
   */
  public abstract Stateful execute(
      Function callable,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      boolean isTail);

  @Specialization
  Stateful invokeUncached(
      Function function,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      boolean isTail,
      @Cached IndirectArgumentSorterNode mappingNode,
      @Cached IndirectCurryNode curryNode,
      @Cached CaptureCallerInfoNode captureCallerInfoNode) {
    CallArgumentInfo.ArgumentMapping argumentMapping =
        CallArgumentInfo.ArgumentMappingBuilder.generate(function.getSchema(), schema);

    ArgumentSorterNode.MappedArguments mappedArguments =
        mappingNode.execute(
            function.getSchema(),
            argumentMapping,
            argumentsExecutionMode,
            function,
            state,
            arguments);

    CallerInfo callerInfo = null;

    if (function.getSchema().getCallerFrameAccess().shouldFrameBePassed()) {
      callerInfo = captureCallerInfoNode.execute(callerFrame.materialize());
    }

    return curryNode.execute(
        callerFrame == null ? null : callerFrame.materialize(),
        function,
        callerInfo,
        mappedArguments.getState(),
        mappedArguments.getSortedArguments(),
        mappedArguments.getOversaturatedArguments(),
        function.getSchema(),
        argumentMapping.getPostApplicationSchema(),
        defaultsExecutionMode,
        argumentsExecutionMode,
        isTail);
  }
}
