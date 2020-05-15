package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.io.BufferedReader;
import java.io.IOException;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition.ExecutionMode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.error.RuntimeError;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "IO.readln", description = "Reads a line from standard in.")
public abstract class ReadlnNode extends BuiltinRootNode {
  public ReadlnNode(Language language) {
    super(language);
  }

  @Specialization
  Stateful doRead(VirtualFrame frame, @CachedContext(Language.class) Context ctx) {
    return read(ctx.getIn(), Function.ArgumentsHelper.getState(frame.getArguments()));
  }

  @TruffleBoundary
  private Stateful read(BufferedReader in, Object state) {
    try {
      String str = in.readLine();

      return new Stateful(state, str);
    } catch (IOException e) {
      return new Stateful(state, new RuntimeError("Empty input stream."));
    }
  }

  /**
   * Creates a {@link Function} object ignoring its first argument and reading from the standard
   * input stream.
   *
   * @param language the current {@link Language} instance
   * @return a {@link Function} object wrapping the behavior of this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        ReadlnNodeGen.create(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ExecutionMode.EXECUTE));
  }

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public String getName() {
    return "IO.readln";
  }
}
