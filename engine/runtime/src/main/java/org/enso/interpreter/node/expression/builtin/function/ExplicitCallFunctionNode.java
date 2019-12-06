package org.enso.interpreter.node.expression.builtin.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * This node implements the built-in functionality for the explicit {@code call} operator on
 * functions.
 *
 * <p>It is a standard builtin node, and hence conforms to the interface for these.
 */
@NodeInfo(shortName = "Function.call", description = "Allows function calls to be made explicitly")
public class ExplicitCallFunctionNode extends BuiltinRootNode {
  private @Child InvokeCallableNode invokeCallableNode;
  private final ConditionProfile isFunctionProfile = ConditionProfile.createCountingProfile();

  private ExplicitCallFunctionNode(Language language) {
    super(language);
    this.invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[0],
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
    this.invokeCallableNode.markTail();
  }

  /**
   * Forces execution of a function.
   *
   * @param frame current execution frame
   * @return the value of executing the function.
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object[] arguments = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    Object thisArg = arguments[0];

    if (isFunctionProfile.profile(TypesGen.isFunction(thisArg))) {
      return invokeCallableNode.execute(thisArg, frame, state, new Object[0]);
    } else {
      throw new RuntimeException("Object not callable: " + thisArg);
    }
  }

  /**
   * Creates a {@link Function} object that forces the execution of the object it is applied to.
   *
   * <p>This behaves in a curried manner, so for some function {@code f} you can call it with
   * arguments where necessary (e.g. {@code f.call a b}.
   *
   * @param language the current {@link Language} instance
   * @return a {@link Function} object wrapping the behavior of this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new ExplicitCallFunctionNode(language),
        FunctionSchema.CallStrategy.DIRECT_WHEN_TAIL,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  @Override
  public String getName() {
    return "Function.call";
  }
}
