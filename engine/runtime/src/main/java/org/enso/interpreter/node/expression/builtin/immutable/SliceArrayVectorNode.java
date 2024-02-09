package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;

@BuiltinMethod(type = "Vector", name = "slice", description = "Returns a slice of this Vector.")
public final class SliceArrayVectorNode extends Node {
  private @Child ArrayLikeLengthNode lengthNode = ArrayLikeLengthNode.create();

  private SliceArrayVectorNode() {}

  public static SliceArrayVectorNode build() {
    return new SliceArrayVectorNode();
  }

  Object execute(Object self, long start, long end) {
    var len = lengthNode.executeLength(self);
    return ArrayLikeHelpers.slice(self, start, end, len);
  }
}
