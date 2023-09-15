package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * Reads and evaluates the expression provided as a function argument. It handles the case where
 * none is given and the default should be used instead.
 */
@NodeInfo(shortName = "ReadArg", description = "Read function argument.")
public final class ReadArgumentNode extends ExpressionNode {
  private final int index;
  @Child ExpressionNode defaultValue;
  @Child ReadArgumentCheckNode checkType;
  private final CountingConditionProfile defaultingProfile = CountingConditionProfile.create();

  private ReadArgumentNode(int position, ExpressionNode defaultValue, ReadArgumentCheckNode check) {
    this.index = position;
    this.defaultValue = defaultValue;
    this.checkType = check;
  }

  /**
   * Creates an instance of this node.
   *
   * @param position the argument's position at the definition site
   * @param defaultValue the default value provided for that argument
   * @param check {@code null} or node to check type of input
   * @return a node representing the argument at position {@code idx}
   */
  public static ReadArgumentNode build(
      int position, ExpressionNode defaultValue, ReadArgumentCheckNode check) {
    return new ReadArgumentNode(position, defaultValue, check);
  }

  ReadArgumentNode plainRead() {
    var node = (ReadArgumentNode) this.copy();
    node.checkType = null;
    return node;
  }

  /**
   * Computes the value of an argument in a function.
   *
   * <p>This function also handles the defaulted case by checking for a {@code null} value at the
   * argument's position. This works in conjunction with {@link
   * org.enso.interpreter.runtime.callable.argument.CallArgumentInfo.ArgumentMapping#reorderAppliedArguments(Object[],
   * Object[])} which will place nulls in any position where an argument has not been applied.
   *
   * @param frame the stack frame to execute in
   * @return the computed value of the argument at this position
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    Object arguments[] = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());

    Object v;
    if (defaultValue == null) {
      v = arguments[index];
    } else {
      // Note [Handling Argument Defaults]
      if (defaultingProfile.profile(arguments.length <= index || arguments[index] == null)) {
        v = defaultValue.executeGeneric(frame);
      } else {
        v = arguments[index];
      }
    }
    if (checkType != null) {
      v = checkType.handleCheckOrConversion(frame, v);
    }
    return v;
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

  @Override
  public boolean isInstrumentable() {
    return false;
  }
}
