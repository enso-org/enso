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
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@BuiltinMethod(
    type = "Meta",
    name = "is_of_type_builtin",
    description = "Checks if the given type matches the expected type.")
public class IsOfTypeMetaNode extends Node {
  private @Child TypesLibrary types =
      TypesLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object tpe, Object expected) {
    if (library.isMetaObject(tpe) && library.isMetaObject(expected)) {
      return library.isIdentical(tpe, expected, InteropLibrary.getUncached());
    } else {
      if (types.hasType(tpe) && types.hasType(expected)) {
        return types.getType(tpe) == types.getType(expected);
      } else {
        Context ctx = Context.get(this);
        return false;
      }
    }
  }
}
