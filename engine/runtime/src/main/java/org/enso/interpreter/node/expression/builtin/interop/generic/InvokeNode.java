package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "invoke",
    description = "Invokes a polyglot method by name, dispatching by the target argument.")
public class InvokeNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object _this, Object target, String name, Array arguments) {
    try {
      return library.invokeMember(target, name, arguments.getItems());
    } catch (UnsupportedMessageException
        | ArityException
        | UnsupportedTypeException
        | UnknownIdentifierException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
