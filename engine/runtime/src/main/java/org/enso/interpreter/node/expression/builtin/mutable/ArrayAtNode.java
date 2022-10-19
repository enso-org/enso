package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Array", name = "at", description = "Get element of a polyglot array")
public class ArrayAtNode extends Node {
  @Child InteropLibrary iop = InteropLibrary.getFactory().createDispatched(3);
  @Child HostValueToEnsoNode convert = HostValueToEnsoNode.build();

  Object execute(Object self, long index) {
    try {
      long actualIndex = index < 0 ? index + iop.getArraySize(self) : index;
      var r = iop.readArrayElement(self, actualIndex);
      return convert.execute(r);
    } catch (UnsupportedMessageException ex) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(ex);
    } catch (InvalidArrayIndexException e) {
      Context ctx = Context.get(this);
      try {
        return DataflowError.withoutTrace(
            ctx.getBuiltins().error().makeIndexOutOfBoundsError(index, iop.getArraySize(self)),
            this);
      } catch (UnsupportedMessageException ex) {
        CompilerDirectives.transferToInterpreter();
        throw new PanicException(ex.getMessage(), this);
      }
    }
  }
}
