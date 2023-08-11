package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Array_Like_Helpers",
    name = "at",
    description = "Returns an element of Vector at the specified index.")
public class AtVectorNode extends Node {
  private @Child ArrayLikeAtNode at = ArrayLikeAtNode.create();
  private @Child ArrayLikeLengthNode length = ArrayLikeLengthNode.create();

  Object execute(Object arrayLike, long index) {
    long len = length.executeLength(arrayLike);
    try {
      long actualIndex = index < 0 ? index + len : index;
      return at.executeAt(arrayLike, actualIndex);
    } catch (InvalidArrayIndexException e) {
      EnsoContext ctx = EnsoContext.get(this);
      return DataflowError.withoutTrace(
          ctx.getBuiltins().error().makeIndexOutOfBounds(index, len), this);
    }
  }
}
