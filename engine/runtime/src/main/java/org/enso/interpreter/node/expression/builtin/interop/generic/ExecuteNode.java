package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Polyglot",
    name = "execute",
    description = "Executes a polyglot function object (e.g. a lambda).")
public class ExecuteNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object _this, Object callable, Vector arguments) {
    try {
      return library.execute(callable, arguments.getItems());
    } catch (UnsupportedMessageException | ArityException | UnsupportedTypeException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
