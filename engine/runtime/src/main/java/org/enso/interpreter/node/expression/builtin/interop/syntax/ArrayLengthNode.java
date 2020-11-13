package org.enso.interpreter.node.expression.builtin.interop.syntax;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Any",
    name = "<polyglot_array_length>",
    description = "Returns the length of a polyglot array.")
public class ArrayLengthNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  public Object execute(Object _this) {
    try {
      return library.getArraySize(_this);
    } catch (UnsupportedMessageException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
