package org.enso.interpreter.node.expression.builtin.debug;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.node.expression.debug.BreakpointNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;

/** Root of the builtin Debug.breakpoint function. */
@NodeInfo(
    shortName = "Debug.breakpoint",
    description = "Root of the builtin Debug.breakpoint function.")
public class DebugBreakpointNode extends BuiltinRootNode {
  private @Child BreakpointNode instrumentableNode = BreakpointNode.build();

  private DebugBreakpointNode(Language language) {
    super(language);
  }

  /**
   * Executes this node by delegating to its instrumentable child.
   *
   * @param frame current execution frame
   * @return the result of running the instrumentable node
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return instrumentableNode.execute(frame, state);
  }

  /**
   * Wraps this node in a 1-argument function.
   *
   * @param language current language instance
   * @return the function wrapper for this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNodeWithCallerFrameAccess(
        new DebugBreakpointNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(
            0, Constants.THIS_ARGUMENT_NAME, ArgumentDefinition.ExecutionMode.EXECUTE));
  }
}
