package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "get_array_size",
    description = "Gets the size of a polyglot array.")
public class GetArraySizeNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object _this, Object array) {
    try {
      return library.getArraySize(array);
    } catch (UnsupportedMessageException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
