package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCopyToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;

@BuiltinMethod(
    type = "Vector",
    name = "insert_builtin",
    description = "Inserts a set of values into the Vector at the specified index.",
    autoRegister = false)
public abstract class InsertBuiltinVectorNode extends Node {
  static InsertBuiltinVectorNode build() {
    return InsertBuiltinVectorNodeGen.create();
  }

  abstract EnsoObject execute(Object vec, long index, Object values);

  @Specialization
  EnsoObject fromObject(
      Object vec,
      long index,
      Object values,
      @Cached ArrayLikeCopyToArrayNode copyNode,
      @Cached ArrayLikeLengthNode lengthNode) {
    long currentLength = lengthNode.executeLength(vec);
    long valuesLength = lengthNode.executeLength(values);
    var result = ArrayLikeHelpers.allocate(currentLength + valuesLength);
    copyNode.execute(vec, 0, result, 0, index);
    copyNode.execute(values, 0, result, index, valuesLength);
    copyNode.execute(vec, index, result, index + valuesLength, currentLength - index);
    return ArrayLikeHelpers.asVectorFromArray(result);
  }
}
