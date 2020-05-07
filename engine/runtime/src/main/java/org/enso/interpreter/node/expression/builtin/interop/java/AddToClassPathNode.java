package org.enso.interpreter.node.expression.builtin.interop.java;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

import java.io.File;

@NodeInfo(shortName = "Java.add_to_class_path", description = "Adds a path to the host class path.")
public abstract class AddToClassPathNode extends BuiltinRootNode {
  AddToClassPathNode(Language language) {
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
        AddToClassPathNodeGen.create(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "path", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  @Specialization
  Stateful doExecute(VirtualFrame frame, @CachedContext(Language.class) Context context) {
    try {
      String arg =
          TypesGen.expectString(
              Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1]);
      Object state = Function.ArgumentsHelper.getState(frame.getArguments());
      context.getEnvironment().addToHostClassPath(context.getTruffleFile(new File(arg)));
      return new Stateful(state, context.getBuiltins().unit());
    } catch (UnexpectedResultException e) {
      throw new PanicException(e.getMessage(), this);
    }
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Java.add_to_class_path";
  }
}
