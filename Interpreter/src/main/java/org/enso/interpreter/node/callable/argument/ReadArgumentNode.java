package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * Reads and evaluates the expression provided as a function argument. It handles the case where
 * none is given and the default should be used instead.
 */
@NodeInfo(description = "Read function argument.")
public class ReadArgumentNode extends ExpressionNode {
  private final int index;
  @Child ExpressionNode defaultValue;
  private final ConditionProfile defaultingProfile = ConditionProfile.createCountingProfile();

  /**
   * Creates a node to compute a function argument.
   *
   * @param position the argument's position at the definition site
   * @param defaultValue the default value provided for that argument
   */
  public ReadArgumentNode(int position, ExpressionNode defaultValue) {
    this.index = position;
    this.defaultValue = defaultValue;
  }

  /**
   * Computes the value of an argument in a function.
   *
   * <p>This function also handles the defaulted case by checking for a {@code null} value at the
   * argument's position. This works in conjunction with {@link
   * org.enso.interpreter.runtime.callable.argument.CallArgumentInfo#reorderArguments(int[],
   * Object[], Object[])}, which will place nulls in any position where an argument has not been
   * applied.
   *
   * @param frame the stack frame to execute in
   * @return the computed value of the argument at this position
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    Object argument = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[index];

    if (defaultValue == null) {
      return argument;
    }

    // Note [Handling Argument Defaults]
    if (defaultingProfile.profile(argument == null)) {
      return defaultValue.executeGeneric(frame);
    } else {
      return argument;
    }
  }

  /* Note [Handling Argument Defaults]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * While it is tempting to handle defaulted arguments as a special case, we instead treat them as
   * the absence of an argument for that position in the function definition.
   *
   * Any `null` value is treated as a usage of the default argument, so when this execution
   * encounters a null value at the argument position, then it will instead execute the default
   * value.
   */
}
