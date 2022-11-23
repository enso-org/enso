package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "execute",
    description = "Executes a polyglot function object (e.g. a lambda).",
    autoRegister = false)
public abstract class ExecuteNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child HostValueToEnsoNode hostValueToEnsoNode = HostValueToEnsoNode.build();
  private final BranchProfile err = BranchProfile.create();

  static ExecuteNode build() {
    return ExecuteNodeGen.create();
  }

  abstract Object execute(Object callable, Object arguments);

  @Specialization
  Object doExecute(Object callable, Object arguments, @Cached("build()") CoerceArrayNode coerce) {
    try {
      return hostValueToEnsoNode.execute(library.execute(callable, coerce.execute(arguments)));
    } catch (UnsupportedMessageException | ArityException | UnsupportedTypeException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
