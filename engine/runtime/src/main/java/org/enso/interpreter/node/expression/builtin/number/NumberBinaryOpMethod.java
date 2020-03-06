package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(shortName = "Number.BinOp", description = "An abstract class for binary ops on numbers.")
public abstract class NumberBinaryOpMethod extends BuiltinRootNode {
  private BranchProfile thatOpBadTypeProfile = BranchProfile.create();

  /**
   * Constructs an instance of this node.
   *
   * @param language the language instance for which the node is being constructed
   */
  protected NumberBinaryOpMethod(Language language) {
    super(language);
  }

  /**
   * Executes this node.
   *
   * @param frame current execution frame
   * @return the result of performing `op` on
   */
  @Override
  public final Stateful execute(VirtualFrame frame) {
    // Note [Safe Casts in Execute]
    long thisArg =
        TypesGen.asLong(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0]);

    Object thatArg = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];

    if (TypesGen.isLong(thatArg)) {
      long thatArgAsLong = TypesGen.asLong(thatArg);
      Object state = Function.ArgumentsHelper.getState(frame.getArguments());

      return new Stateful(state, op(thisArg, thatArgAsLong));
    } else {
      thatOpBadTypeProfile.enter();
      throw new TypeError("Unexpected type for `that` operand in " + getName(), this);
    }
  }

  /* Note [Safe Casts in Execute]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * It is safe to perform this cast here as the type of that argument has already been checked by
   * the method dispatch mechanism. If this `execute` method is being called, we know that the
   * `this` argument must be of the correct type.
   *
   * This does not hold for any remaining arguments, and as such they must be checked.
   */

  /**
   * The binary operation embodied by the subclass of this node.
   *
   * @param thisArg the left operand (this)
   * @param thatArg the right operand (that)
   * @return the result of {@code this `op` that}
   */
  protected abstract long op(long thisArg, long thatArg);

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Number.BinOp";
  }
}
