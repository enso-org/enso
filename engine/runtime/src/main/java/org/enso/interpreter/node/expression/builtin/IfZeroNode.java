package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.argument.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/** The root node for the basic control flow structure of the language. */
@NodeInfo(shortName = "Number.ifZero", description = "Root node of the Number.ifZero method.")
public class IfZeroNode extends BuiltinRootNode {
  private @Child ThunkExecutorNode leftThunkExecutorNode = ThunkExecutorNode.build(true);
  private @Child ThunkExecutorNode rightThunkExecutorNode = ThunkExecutorNode.build(true);
  private final ConditionProfile condProfile = ConditionProfile.createCountingProfile();

  private IfZeroNode(Language language) {
    super(language);
  }

  /**
   * Takes a number and two thunks and executes the first thunk if the number was 0 and second
   * otherwise.
   *
   * <p>Assumes the number is the first argument in the current execution frame, while the first and
   * second thunks are respectively the second and third arguments.
   *
   * @param frame current execution frame
   * @return the result of executing the proper thunk
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    long self =
        TypesGen.asLong(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0]);
    Thunk ifT =
        TypesGen.asThunk(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1]);
    Thunk ifF =
        TypesGen.asThunk(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[2]);
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    if (condProfile.profile(self == 0)) {
      return leftThunkExecutorNode.executeThunk(ifT, state);
    } else {
      return rightThunkExecutorNode.executeThunk(ifF, state);
    }
  }

  /**
   * Creates a three-argument function wrapping this node.
   *
   * @param language current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new IfZeroNode(language),
        FunctionSchema.CallStrategy.DIRECT_WHEN_TAIL,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "ifTrue", ArgumentDefinition.ExecutionMode.PASS_THUNK),
        new ArgumentDefinition(2, "ifFalse", ArgumentDefinition.ExecutionMode.PASS_THUNK));
  }
}
