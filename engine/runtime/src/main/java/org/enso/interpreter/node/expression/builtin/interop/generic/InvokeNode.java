package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(
    shortName = "Polyglot.invoke",
    description = "Invokes a polyglot method by name, dispatching by the target argument.")
public class InvokeNode extends BuiltinRootNode {
  private InvokeNode(Language language) {
    super(language);
  }

  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  /**
   * Creates a function wrapping this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new InvokeNode(language),
        CallStrategy.ALWAYS_DIRECT,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "target", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(2, "method_name", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(3, "arguments", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Executes the node.
   *
   * @param frame current execution frame.
   * @return the result of converting input into a string.
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    Object[] args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
    Object callable = args[1];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    try {
      String method = TypesGen.expectString(args[2]);
      Object[] arguments = TypesGen.expectVector(args[3]).getItems();
      Object res = library.invokeMember(callable, method, arguments);
      return new Stateful(state, res);
    } catch (UnsupportedMessageException
        | ArityException
        | UnsupportedTypeException
        | UnexpectedResultException
        | UnknownIdentifierException e) {
      err.enter();
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
    return "Polyglot.invoke";
  }
}
