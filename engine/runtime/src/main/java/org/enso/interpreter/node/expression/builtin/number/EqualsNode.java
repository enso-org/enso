package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(shortName = "Number.==", description = "Equality on numbers.")
public class EqualsNode extends BuiltinRootNode {
  private EqualsNode(Language language) {
    super(language);
  }

  private @CompilerDirectives.CompilationFinal AtomConstructor tru;
  private @CompilerDirectives.CompilationFinal AtomConstructor fls;
  private final BranchProfile thatOpBadTypeProfile = BranchProfile.create();

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new EqualsNode(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "that", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Executes the node.
   *
   * @param frame current execution frame
   * @return whether or not the input arguments are equal
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    long thisArg =
        TypesGen.asLong(Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0]);

    Object thatArg = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];

    if (TypesGen.isLong(thatArg)) {
      long thatArgAsLong = TypesGen.asLong(thatArg);
      Object state = Function.ArgumentsHelper.getState(frame.getArguments());

      return new Stateful(state, thisArg == thatArgAsLong);
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
    return "Number.==";
  }
}
