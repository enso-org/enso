package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Array", name = "length", description = "Length of polyglot array")
public class ArrayLengthNode extends Node {
  @Child InteropLibrary iop = InteropLibrary.getFactory().createDispatched(3);

  long execute(Object self) {
    try {
      return iop.getArraySize(self);
    } catch (UnsupportedMessageException ex) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(ex);
    }
  }
}
