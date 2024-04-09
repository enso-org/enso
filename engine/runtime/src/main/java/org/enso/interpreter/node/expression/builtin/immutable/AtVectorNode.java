package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Array_Like_Helpers",
    name = "at",
    description = "Returns an element of Vector at the specified index.")
public class AtVectorNode extends Node {
  private @Child ArrayLikeAtNode at = ArrayLikeAtNode.create();
  private @Child ArrayLikeLengthNode length;

  Object execute(Object arrayLike, long index) {
    try {
      long actualIndex = index < 0 ? index + len(arrayLike) : index;
      return at.executeAt(arrayLike, actualIndex);
    } catch (InvalidArrayIndexException e) {
      var len = len(arrayLike);
      var ctx = EnsoContext.get(this);
      var payload = ctx.getBuiltins().error().makeIndexOutOfBounds(index, len);
      return DataflowError.withTrace(payload, new PanicException(payload, this));
    }
  }

  private long len(Object arrayLike) {
    if (length == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      length = insert(ArrayLikeLengthNode.create());
    }
    return length.executeLength(arrayLike);
  }
}
