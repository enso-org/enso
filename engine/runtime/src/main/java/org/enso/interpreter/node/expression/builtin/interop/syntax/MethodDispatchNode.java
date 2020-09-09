package org.enso.interpreter.node.expression.builtin.interop.syntax;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(shortName = "<polyglot_dispatch>", description = "Invokes a polyglot method by name.")
@ReportPolymorphism
public abstract class MethodDispatchNode extends BuiltinRootNode {
  MethodDispatchNode(Language language) {
    super(language);
  }

  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  /**
   * Creates an instance of this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static MethodDispatchNode build(Language language) {
    return MethodDispatchNodeGen.create(language);
  }

  /**
   * Executes the node.
   *
   * @param frame current execution frame.
   * @return the result of converting input into a string.
   */
  @Specialization(guards = "symbol == cachedSymbol")
  public Stateful run(
      VirtualFrame frame,
      @Bind("getSymbol(frame)") UnresolvedSymbol symbol,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("buildToArray(cachedSymbol)") UnresolvedSymbol toArray) {
    Object[] args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
    Object callable = args[0];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    Object arguments = args[2];
    Stateful casted = invokeCallableNode.execute(toArray, frame, state, new Object[] {arguments});
    try {
      Object[] castedArgs = TypesGen.expectArray(casted.getValue()).getItems();
      Object res = library.invokeMember(callable, symbol.getName(), castedArgs);
      return new Stateful(casted.getState(), res);
    } catch (UnsupportedMessageException
        | ArityException
        | UnsupportedTypeException
        | UnexpectedResultException
        | UnknownIdentifierException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }

  UnresolvedSymbol buildToArray(UnresolvedSymbol originalSymbol) {
    return UnresolvedSymbol.build("to_array", originalSymbol.getScope());
  }

  UnresolvedSymbol getSymbol(VirtualFrame frame) {
    return TypesGen.asUnresolvedSymbol(
        Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1]);
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "<polyglot_dispatch>";
  }
}
