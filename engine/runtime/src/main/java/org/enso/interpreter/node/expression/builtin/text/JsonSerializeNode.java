package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.builtin.LanguageEntitySerializer;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.state.Stateful;

/** An implementation of generic JSON serialization. */
@NodeInfo(shortName = "Any.json_serialize", description = "Generic JSON serialization.")
public class JsonSerializeNode extends BuiltinRootNode {
  private JsonSerializeNode(Language language) {
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
        new JsonSerializeNode(language),
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
    return new Stateful(state, serialize(thisArg));
  }

  @CompilerDirectives.TruffleBoundary
  private String serialize(Object obj) {
    return LanguageEntitySerializer.serialize(obj);
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Any.json_serialize";
  }
}
