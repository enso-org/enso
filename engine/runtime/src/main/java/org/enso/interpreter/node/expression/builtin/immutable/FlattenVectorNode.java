package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CopyNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Vector",
    name = "flatten",
    description = "Flattens a vector of vectors into a single vector.",
    autoRegister = false)
public class FlattenVectorNode extends Node {
  private @Child InteropLibrary interop = InteropLibrary.getFactory().createDispatched(3);
  private @Child CopyNode copyNode = CopyNode.build();

  Object execute(Vector self) {
    try {
      return flatten(self);
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(e);
    }
  }

  private Object flatten(Vector self)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    long length = self.length(interop);
    var storage = self.toArray();

    long flattened_length = 0;
    for (long i = 0; i < length; i++) {
      var item = interop.readArrayElement(storage, i);
      if (!interop.hasArrayElements(item)) {
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        throw new PanicException(builtins.error().makeTypeError(builtins.array(), item, "["+i+"]"), this);
      }

      flattened_length += interop.getArraySize(item);
    }

    Array result = Array.allocate(flattened_length);
    long current_index = 0;
    for (long i = 0; i < length; i++) {
      var item = interop.readArrayElement(storage, i);
      var item_length = interop.getArraySize(item);
      copyNode.execute(item, 0, result, current_index, item_length);
      current_index += item_length;
    }
    return Vector.fromArray(result);
  }
}
