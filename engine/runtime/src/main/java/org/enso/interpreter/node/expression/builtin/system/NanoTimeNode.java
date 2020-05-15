package org.enso.interpreter.node.expression.builtin.system;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "System.nano_time", description = "Gets the nanosecond resolution system time.")
public final class NanoTimeNode extends BuiltinRootNode {
  private NanoTimeNode(Language language) {
    super(language);
  }

  /**
   * Creates a {@link Function} object wrapping this object.
   *
   * @param language the current {@link Language} instance
   * @return a {@link Function} object wrapping the behavior of this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new NanoTimeNode(language),
        FunctionSchema.CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  @Override
  public Stateful execute(VirtualFrame frame) {
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, getNanoTime());
  }

  @CompilerDirectives.TruffleBoundary
  private long getNanoTime() {
    return System.nanoTime();
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public String getName() {
    return "System.nano_time";
  }
}
