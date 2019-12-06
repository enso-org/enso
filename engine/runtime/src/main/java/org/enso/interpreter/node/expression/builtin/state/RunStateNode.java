package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.argument.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/** Root for the builtin State.run function. */
@NodeInfo(shortName = "State.run", description = "Root for the builtin State.run function")
public class RunStateNode extends BuiltinRootNode {
  private RunStateNode(Language language) {
    super(language);
  }

  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build(false);
  private final ConditionProfile thunksProfile = ConditionProfile.createCountingProfile();

  /**
   * Runs a stateful computation ({@link org.enso.interpreter.runtime.callable.argument.Thunk}) with
   * a local state value, without modifying the caller state.
   *
   * <p>Assumes the local state value is the second argument, while the stateful computation is the
   * third argument.
   *
   * @param frame current execution frame
   * @return the result of running the stateful computation with the desired initial state
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object localState = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];
    Object maybeThunk = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[2];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    if (thunksProfile.profile(TypesGen.isThunk(maybeThunk))) {
      return new Stateful(
          state,
          thunkExecutorNode
              .executeThunk(TypesGen.asThunk(maybeThunk), localState)
              .getValue());
    } else {
      return new Stateful(state, maybeThunk);
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
        new RunStateNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "state", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(
            2, "statefulComputation", ArgumentDefinition.ExecutionMode.PASS_THUNK));
  }

  @Override
  public String getName() {
    return "State.run";
  }
}
