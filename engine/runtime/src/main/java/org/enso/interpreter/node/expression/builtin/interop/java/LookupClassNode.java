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

@NodeInfo(shortName = "Java.lookup_class", description = "Looks up a Java symbol.")
public abstract class LookupClassNode extends BuiltinRootNode {
  LookupClassNode(Language language) {
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
        LookupClassNodeGen.create(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "name", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  @Specialization
  Stateful doExecute(VirtualFrame frame, @CachedContext(Language.class) Context ctx) {
    try {
      String arg =
          TypesGen.expectString(
              Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1]);
      Object state = Function.ArgumentsHelper.getState(frame.getArguments());
      Object res = ctx.getEnvironment().lookupHostSymbol(arg);
      return new Stateful(state, res);
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
    return "Java.lookup_class";
  }
}
