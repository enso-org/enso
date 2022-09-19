package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@BuiltinMethod(
    type = "Meta",
    name = "type_of_builtin",
    description = "Returns the type of a value.")
public class TypeOfNode extends Node {
  private @Child TypesLibrary types =
      TypesLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object value) {
    if (types.hasType(value)) {
      return types.getType(value);
    } else {
      Context ctx = Context.get(this);
      return ctx.getBuiltins().any();
    }
  }
}
