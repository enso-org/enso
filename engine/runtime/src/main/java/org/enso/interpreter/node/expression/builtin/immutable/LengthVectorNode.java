package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;

@BuiltinMethod(
    type = "Array_Like_Helpers",
    name = "length",
    description = "Returns the length of this Vector.")
public class LengthVectorNode extends Node {
  @Child ArrayLikeLengthNode length = ArrayLikeLengthNode.create();

  long execute(@AcceptsWarning Object arrayLike) {
    return length.executeLength(arrayLike);
  }
}
