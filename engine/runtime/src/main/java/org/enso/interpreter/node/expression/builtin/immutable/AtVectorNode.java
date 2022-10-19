package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Vector",
    name = "at",
    description = "Returns an element of Vector at the specified index.")
public class AtVectorNode extends Node {
  private @Child InteropLibrary interop = InteropLibrary.getFactory().createDispatched(3);
  private @Child HostValueToEnsoNode toEnso = HostValueToEnsoNode.build();

  Object execute(Vector self, long index) {
    try {
      long actualIndex = index < 0 ? index + self.length(interop) : index;
      return self.readArrayElement(actualIndex, interop, toEnso);
    } catch (InvalidArrayIndexException e) {
      Context ctx = Context.get(this);
      try {
        return DataflowError.withoutTrace(
            ctx.getBuiltins().error().makeIndexOutOfBoundsError(index, self.length(interop)), this);
      } catch (UnsupportedMessageException ex) {
        CompilerDirectives.transferToInterpreter();
        throw new PanicException(e.getMessage(), this);
      }
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
