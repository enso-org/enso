package org.enso.interpreter.node.expression.builtin.interop.syntax;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Any",
    name = "<polyglot_array_at>",
    description = "Returns the element of a polyglot array at a given index.")
public class GetArrayElementNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child HostValueToEnsoNode hostValueToEnsoNode = HostValueToEnsoNode.build();
  private final BranchProfile err = BranchProfile.create();

  public Object execute(Object _this, long index) {
    try {
      return hostValueToEnsoNode.execute(library.readArrayElement(_this, index));
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
