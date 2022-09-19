package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "type_of_polyglot_builtin",
    description = "Returns the type of a polyglot value.")
public class TypeOfPolyglotNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object value) {
    if (library.hasMetaObject(value)) {
      try {
        return library.getMetaObject(value);
      } catch (UnsupportedMessageException e) {
        err.enter();
        Builtins builtins = Context.get(this).getBuiltins();
        throw new PanicException(
            builtins.error().makeTypeError(builtins.any(), value, "object"), this);
      }
    } else {
      Context ctx = Context.get(this);
      return ctx.getBuiltins().any();
    }
  }
}
