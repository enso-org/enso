package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * This node handles optimising calls. It performs detection based on the kind of call being made,
 * and then executes the call in the most efficient manner possible for it.
 */
@NodeInfo(shortName = "CallOptimiser", description = "Optimises function calls")
public abstract class CallOptimiserNode extends Node {

  CallOptimiserNode() {}

  /**
   * Creates an instance of default implementation of {@link CallOptimiserNode}.
   *
   * @return a fresh instance of {@link CallOptimiserNode}
   */
  public static CallOptimiserNode build() {
    return SimpleCallOptimiserNode.build();
  }

  /**
   * Calls the provided {@code callable} using the provided {@code arguments}.
   *
   * @param callable the callable to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to {@code callable}
   * @return the result of executing {@code callable} using {@code arguments}
   */
  public abstract Stateful executeDispatch(
      Function callable, CallerInfo callerInfo, Object state, Object[] arguments);
}
