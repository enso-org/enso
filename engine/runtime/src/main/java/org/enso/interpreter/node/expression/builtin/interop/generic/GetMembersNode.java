package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

@BuiltinMethod(
    type = "Polyglot",
    name = "get_members",
    description = "Returns a polyglot array of the object's member names.",
    autoRegister = false)
public class GetMembersNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);

  Object execute(Object object) {
    try {
      return library.getMembers(object);
    } catch (UnsupportedMessageException e) {
      return ArrayLikeHelpers.empty();
    }
  }
}
