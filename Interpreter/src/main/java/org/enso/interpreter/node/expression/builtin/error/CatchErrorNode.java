package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/** Root node for the {@code catch} function. */
@NodeInfo(shortName = "Error.catch", description = "Root node for the catch function.")
public class CatchErrorNode extends RootNode {
  private @Child InvokeCallableNode invokeCallableNode;
  private final ConditionProfile executionProfile = ConditionProfile.createCountingProfile();

  private CatchErrorNode(Language language) {
    super(language);
    this.invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  /**
   * Executes the logic. Assumes the scrutinee is the first argument to the enclosing function and
   * the handler is the second. If the scrutinee is an error, the function is called with the error
   * payload. Otherwise, it's a no-op.
   *
   * @param frame current execution frame
   * @return the result of calling the handler function
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object[] arguments = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    Object scrutinee = arguments[0];
    Object handler = arguments[1];
    if (executionProfile.profile(TypesGen.isRuntimeError(scrutinee))) {
      return invokeCallableNode.execute(
          handler, state, new Object[] {TypesGen.asRuntimeError(scrutinee).getPayload()});
    } else {
      return new Stateful(state, scrutinee);
    }
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromRootNode(
        new CatchErrorNode(language),
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "handler", ArgumentDefinition.ExecutionMode.EXECUTE));
  }
}
