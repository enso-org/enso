package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(
    type = "Vector",
    name = "remove_builtin",
    description = "Removes a value for the vector at the specified index.",
    autoRegister = false)
public class RemoveAtVectorNode extends Node {
  private @Child InteropLibrary interop = InteropLibrary.getFactory().createDispatched(3);
  private @Child CoerceArrayNode coerceArrayNode = CoerceArrayNode.build();

  Object execute(Vector self, long index) {
    try {
      return removeAtIndex(self, index);
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(e);
    }
  }

  private Object removeAtIndex(Vector self, long index) throws UnsupportedMessageException {
    long actualIndex = index < 0 ? index + self.length(interop) : index;
    Object[] storage = coerceArrayNode.execute(self);
    Object[] result = new Object[storage.length - 1];
    System.arraycopy(storage, 0, result, 0, (int) actualIndex);
    System.arraycopy(storage, (int) actualIndex + 1, result, (int) actualIndex, storage.length - (int) actualIndex - 1);
    return Vector.fromArray(new Array(result));
  }
}
