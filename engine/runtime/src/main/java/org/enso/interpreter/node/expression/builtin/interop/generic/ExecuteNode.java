package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "execute",
    description = "Executes a polyglot function object (e.g. a lambda).")
public class ExecuteNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child HostValueToEnsoNode hostValueToEnsoNode = HostValueToEnsoNode.build();
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object callable, Array arguments) {
    try {
      return hostValueToEnsoNode.execute(library.execute(callable, arguments.getItems()));
    } catch (UnsupportedMessageException | ArityException | UnsupportedTypeException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
