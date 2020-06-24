package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/** The root node for the basic control flow structure of the language. */
@NodeInfo(
    shortName = "Boolean.if_then_else",
    description = "Performs the standard if-then-else control flow operation.")
public class IfThenElseNode extends BuiltinRootNode {
  private @Child ThunkExecutorNode leftThunkExecutorNode = ThunkExecutorNode.build();
  private @Child ThunkExecutorNode rightThunkExecutorNode = ThunkExecutorNode.build();
  private final ConditionProfile condProfile = ConditionProfile.createCountingProfile();

  private IfThenElseNode(Language language) {
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
    boolean self =
        TypesGen.asBoolean(
            Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0]);
    Thunk ifT =
        TypesGen.asThunk(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1]);
    Thunk ifF =
        TypesGen.asThunk(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[2]);
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    if (condProfile.profile(self)) {
      return leftThunkExecutorNode.executeThunk(ifT, state, true);
    } else {
      return rightThunkExecutorNode.executeThunk(ifF, state, true);
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
        new IfThenElseNode(language),
        FunctionSchema.CallStrategy.DIRECT_WHEN_TAIL,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "if_true", ArgumentDefinition.ExecutionMode.PASS_THUNK),
        new ArgumentDefinition(2, "if_false", ArgumentDefinition.ExecutionMode.PASS_THUNK));
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public String getName() {
    return "Boolean.if_then_else";
  }
}
