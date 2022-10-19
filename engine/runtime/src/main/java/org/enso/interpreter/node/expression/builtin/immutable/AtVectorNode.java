package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Vector",
    name = "at_builtin",
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
      throw new PanicException(
          ctx.getBuiltins().error().makeInvalidArrayIndexError(self, index), this);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(e.getMessage(), this);
    }
  }
}
