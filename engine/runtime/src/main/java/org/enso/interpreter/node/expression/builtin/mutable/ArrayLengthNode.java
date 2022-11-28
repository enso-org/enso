package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Array", name = "length", description = "Length of polyglot array")
public class ArrayLengthNode extends Node {
  @Child InteropLibrary iop = InteropLibrary.getFactory().createDispatched(3);

  long execute(Object self) {
    try {
      return iop.getArraySize(self);
    } catch (UnsupportedMessageException ex) {
      var ctx = Context.get(this);
      var pay =
          ctx.getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(new Object[] {self}, ex.getMessage());
      throw new PanicException(pay, this);
    }
  }
}
