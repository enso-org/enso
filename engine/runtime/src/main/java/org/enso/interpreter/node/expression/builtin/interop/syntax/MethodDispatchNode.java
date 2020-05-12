package org.enso.interpreter.node.expression.builtin.interop.syntax;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@NodeInfo(shortName = "<polyglot_dispatch>", description = "Invokes a polyglot method by name.")
public class MethodDispatchNode extends BuiltinRootNode {
  private MethodDispatchNode(Language language) {
    super(language);
  }

  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  /**
   * Creates an instance of this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static MethodDispatchNode build(Language language) {
    return new MethodDispatchNode(language);
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
    Object callable = args[0];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    try {
      String method = TypesGen.expectString(args[1]);
      Object[] arguments = TypesGen.expectVector(args[2]).getItems();
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
    return "<polyglot_dispatch>";
  }
}
