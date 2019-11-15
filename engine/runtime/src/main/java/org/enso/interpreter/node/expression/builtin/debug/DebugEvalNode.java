package org.enso.interpreter.node.expression.builtin.debug;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.node.expression.debug.EvalNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;

/** Root node for the builtin Debug.eval function. */
@NodeInfo(shortName = "Debug.eval", description = "Root node for the builtin Debug.eval function")
public class DebugEvalNode extends BuiltinRootNode {

  private @Child EvalNode evalNode = EvalNode.build();

  private DebugEvalNode(Language language) {
    super(language);
    evalNode.markTail();
  }

  /**
   * Executes the function in the given execution frame.
   *
   * <p>Requires a non-null {@link CallerInfo} passed to it. The string argument containing code to
   * evaluate is this function's second (non-this) argument.
   *
   * @param frame current execution frame
   * @return
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    CallerInfo callerInfo = Function.ArgumentsHelper.getCallerInfo(frame.getArguments());
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    String code = (String) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    return evalNode.execute(callerInfo, state, code);
  }

  /**
   * Returns a two argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node's logic
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNodeWithCallerFrameAccess(
        new DebugEvalNode(language),
        FunctionSchema.CallStrategy.DIRECT_WHEN_TAIL,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "expression", ArgumentDefinition.ExecutionMode.EXECUTE));
  }
}
