package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.node.expression.builtin.number.ModNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(shortName = "Boolean.&&", description = "Computes the logical AND of two booleans")
public class AndNode extends BuiltinRootNode {
  private final BranchProfile thatOpBadTypeProfile = BranchProfile.create();

  private AndNode(Language language) {
    super(language);
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new AndNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "that", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Executes this node.
   *
   * @param frame current execution frame
   * @return the result of performing `op` on the operands
   */
  @Override
  public final Stateful execute(VirtualFrame frame) {
    boolean thisArg =
        TypesGen.asBoolean(
            Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0]);

    Object thatArg = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];

    if (TypesGen.isBoolean(thatArg)) {
      boolean thatArgBool = TypesGen.asBoolean(thatArg);
      Object state = Function.ArgumentsHelper.getState(frame.getArguments());

      return new Stateful(state, thisArg && thatArgBool);
    } else {
      thatOpBadTypeProfile.enter();
      throw new TypeError("Unexpected type for `that` operand in " + getName(), this);
    }
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Boolean.||";
  }
}
