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
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;

@BuiltinMethod(
    type = "Polyglot",
    name = "new",
    description = "Instantiates a polyglot constructor.",
    autoRegister = false)
public abstract class InstantiateNode extends Node {
  @Child private HostValueToEnsoNode fromHost = HostValueToEnsoNode.build();
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  static InstantiateNode build() {
    return InstantiateNodeGen.create();
  }

  abstract Object execute(Object constructor, Object arguments);

  @Specialization
  Object doExecute(
      Object constructor, Object arguments, @Cached("build()") ArrayLikeCoerceToArrayNode coerce) {
    try {
      var value = library.instantiate(constructor, coerce.execute(arguments));
      return fromHost.execute(value);
    } catch (UnsupportedMessageException | ArityException | UnsupportedTypeException e) {
      err.enter();
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(this, null, e);
    }
  }
}
