package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(type = "Array", name = "at", description = "Get element of a polyglot array")
public class ArrayAtNode extends Node {
  @Child InteropLibrary iop = InteropLibrary.getFactory().createDispatched(3);
  @Child HostValueToEnsoNode convert = HostValueToEnsoNode.build();

  Object execute(Object self, long index) {
    try {
      return readElement(self, index);
    } catch (UnsupportedMessageException ex) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(ex);
    }
  }

  private Object readElement(Object self, long index) throws UnsupportedMessageException {
    try {
      long actualIndex = index < 0 ? index + iop.getArraySize(self) : index;
      var element = iop.readArrayElement(self, actualIndex);
      return convert.execute(element);
    } catch (InvalidArrayIndexException e) {
      EnsoContext ctx = EnsoContext.get(this);
      return DataflowError.withoutTrace(
          ctx.getBuiltins().error().makeIndexOutOfBounds(index, iop.getArraySize(self)), this);
    }
  }
}
