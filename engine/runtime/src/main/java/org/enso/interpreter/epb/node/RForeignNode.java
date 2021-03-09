package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;

@NodeField(name = "foreignFunction", type = Object.class)
public abstract class RForeignNode extends ForeignFunctionCallNode {

  abstract Object getForeignFunction();

  @Specialization
  public Object doExecute(
      Object[] arguments, @CachedLibrary("foreignFunction") InteropLibrary interopLibrary) {
    try {
      return interopLibrary.execute(getForeignFunction(), arguments);
    } catch (UnsupportedMessageException | UnsupportedTypeException | ArityException e) {
      throw new IllegalStateException("R parser returned a malformed object", e);
    }
  }
}
