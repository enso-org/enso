package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;

@NodeField(name = "foreignFunction", type = Object.class)
public abstract class RForeignNode extends ForeignFunctionCallNode {

  private @Child CoercePrimitiveNode coercePrimitiveNode = CoercePrimitiveNode.build();

  abstract Object getForeignFunction();

  @Specialization
  public Object doExecute(
      Object[] arguments, @CachedLibrary("foreignFunction") InteropLibrary interopLibrary)
      throws InteropException {
    return coercePrimitiveNode.execute(interopLibrary.execute(getForeignFunction(), arguments));
  }
}
