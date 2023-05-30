package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.expression.builtin.meta.IsValueOfTypeNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

/**
 * Reads and evaluates the expression provided as a function argument. It handles the case where
 * none is given and the default should be used instead.
 */
@NodeInfo(shortName = "ReadArg", description = "Read function argument.")
public class ReadArgumentNode extends ExpressionNode {
  private final String name;
  private final int index;
  @Child ExpressionNode defaultValue;
  @Child IsValueOfTypeNode checkType;
  private final ConditionProfile defaultingProfile = ConditionProfile.createCountingProfile();
  // XXX: Type in a Node is wrong!!!!!
  @CompilerDirectives.CompilationFinal(dimensions = 1)
  private final Type[] expectedTypes;

  private ReadArgumentNode(String name, int position, ExpressionNode defaultValue, Type[] expectedTypes) {
    this.name = name;
    this.index = position;
    this.defaultValue = defaultValue;
    this.expectedTypes = expectedTypes;
    if (expectedTypes != null) {
      checkType = IsValueOfTypeNode.build();
    }
  }

  /**
   * Creates an instance of this node.
   *
   * @param name argument name
   * @param position the argument's position at the definition site
   * @param defaultValue the default value provided for that argument
   * @param expectedTypes {@code null} or expected types to check input for
   * @return a node representing the argument at position {@code idx}
   */
  public static ReadArgumentNode build(
      String name, int position, ExpressionNode defaultValue, List<Type> expectedTypes) {
    var arr =
        expectedTypes == null || expectedTypes.isEmpty()
            ? null
            : expectedTypes.toArray(Type[]::new);
    return new ReadArgumentNode(name, position, defaultValue, arr);
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
  @ExplodeLoop
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
      for (Type t : expectedTypes) {
        if (checkType.execute(t, v)) {
          return v;
        }
      }
      if (!(v instanceof DataflowError) && !(v instanceof Function fn && fn.isThunk())) {
        CompilerDirectives.transferToInterpreter();
        var ctx = EnsoContext.get(this);
        var expecting =
            expectedTypes.length == 1 ? expectedTypes[0] : Arrays.stream(expectedTypes).map(Type::toString).collect(Collectors.joining(" | "));
        var argName = name != null ? name : "Argument #" + (index + 1);
        var err = ctx.getBuiltins().error().makeTypeError(expecting, v, argName);
        throw new PanicException(err, this);
      }
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
}
