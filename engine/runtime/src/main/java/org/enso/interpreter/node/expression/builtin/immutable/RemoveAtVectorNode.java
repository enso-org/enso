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
    name = "remove_builtin",
    description = "Removes a value for the vector at the specified index.",
    autoRegister = false)
public abstract class RemoveAtVectorNode extends Node {
  static RemoveAtVectorNode build() {
    return RemoveAtVectorNodeGen.create();
  }

  abstract EnsoObject execute(Object vec, long index);

  @Specialization
  EnsoObject removeAtIndex(
      Object storage,
      long index,
      @Cached ArrayLikeCopyToArrayNode copyArrayNode,
      @Cached ArrayLikeLengthNode lengthNode) {
    long length = lengthNode.executeLength(storage);
    long actualIndex = index < 0 ? index + length : index;
    var array = ArrayLikeHelpers.allocate(length - 1);
    copyArrayNode.execute(storage, 0, array, 0, actualIndex);
    copyArrayNode.execute(storage, actualIndex + 1, array, actualIndex, length - actualIndex - 1);
    return ArrayLikeHelpers.asVectorFromArray(array);
  }
}
