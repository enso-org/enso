package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/** An implementation of generic string conversion. */
@NodeInfo(shortName = "Any.to_text", description = "Generic text conversion.")
public class AnyToTextNode extends BuiltinRootNode {
  private AnyToTextNode(Language language) {
    super(language);
  }

  /**
   * Creates a function wrapping this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new AnyToTextNode(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Executes the node.
   *
   * @param frame current execution frame.
   * @return the result of converting input into a string.
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object thisArg = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, toText(thisArg));
  }

  @CompilerDirectives.TruffleBoundary
  private String toText(Object txt) {
    return txt.toString();
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Any.to_text";
  }
}
