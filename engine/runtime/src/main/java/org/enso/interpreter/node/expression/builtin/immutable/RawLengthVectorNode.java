package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;

@BuiltinMethod(
    type = "Array_Like_Helpers",
    name = "raw_length",
    description = "Returns the length of this Vector.")
final class RawLengthVectorNode extends Node {
  @Child ArrayLikeLengthNode length = ArrayLikeLengthNode.create();

  final Object execute(@AcceptsWarning Object arrayLike) {
    return length.executeLength(arrayLike);
  }
}
