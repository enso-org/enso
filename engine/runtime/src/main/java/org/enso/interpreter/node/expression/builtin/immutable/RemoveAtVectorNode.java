package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CopyNode;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(
    type = "Vector",
    name = "remove_builtin",
    description = "Removes a value for the vector at the specified index.",
    autoRegister = false)
public class RemoveAtVectorNode extends Node {
  private @Child InteropLibrary interop = InteropLibrary.getFactory().createDispatched(3);
  private @Child CopyNode copyArrayNode = CopyNode.build();

  Object execute(Vector self, long index) {
    try {
      return removeAtIndex(self, index);
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(e);
    }
  }

  private Object removeAtIndex(Vector self, long index) throws UnsupportedMessageException {
    long length = self.length(interop);
    long actualIndex = index < 0 ? index + length : index;
    Object storage = self.toArray();
    Array array = Array.allocate(length - 1);
    copyArrayNode.execute(storage, 0, array, 0, actualIndex);
    copyArrayNode.execute(storage, actualIndex + 1, array, actualIndex, length - actualIndex - 1);
    return Vector.fromArray(array);
  }
}
