package org.enso.interpreter.epb;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.nodes.DirectCallNode;

class GenericForeignNode extends ForeignFunctionCallNode {
  private @Child DirectCallNode callNode;
  private @Child CoercePrimitiveNode coerceNode;

  GenericForeignNode(CallTarget ct) {
    callNode = DirectCallNode.create(ct);
    coerceNode = CoercePrimitiveNode.build();
  }

  @Override
  public Object execute(Object[] arguments) throws InteropException {
    var rawResult = callNode.call(arguments);
    var res = coerceNode.execute(rawResult);
    return res;
  }
}
