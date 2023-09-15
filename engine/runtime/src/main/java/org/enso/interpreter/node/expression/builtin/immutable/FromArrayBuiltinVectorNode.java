package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

@BuiltinMethod(
    type = "Vector",
    name = "from_array",
    description = "Creates a Vector by copying Array content.",
    autoRegister = false)
public abstract class FromArrayBuiltinVectorNode extends Node {
  static FromArrayBuiltinVectorNode build() {
    return FromArrayBuiltinVectorNodeGen.create();
  }

  abstract EnsoObject execute(Object arr);

  @Specialization(guards = "isImmutableArrayLike(arr)")
  EnsoObject fromVector(Object arr) {
    return (EnsoObject) arr;
  }

  @Fallback
  EnsoObject fromArrayLikeObject(Object arr, @Cached ArrayLikeCoerceToArrayNode coerce) {
    return ArrayLikeHelpers.asVectorWithCheckAt(coerce.execute(arr));
  }

  static boolean isImmutableArrayLike(Object obj) {
    return ArrayLikeHelpers.isImmutable(obj);
  }
}
