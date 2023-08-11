package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.Vector;
import org.enso.interpreter.runtime.error.PanicException;

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

  @Specialization
  Vector fromVector(Vector arr) {
    return arr;
  }

  @Specialization(guards = "interop.hasArrayElements(arr)")
  EnsoObject fromArrayLikeObject(
      Object arr,
      @Cached ArrayLikeCoerceToArrayNode coerce,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return ArrayLikeHelpers.asVectorWithCheckAt(coerce.execute(arr));
  }

  @Fallback
  Vector fromUnknown(Object arr) {
    throw unsupportedException(arr);
  }

  private PanicException unsupportedException(Object arr) {
    var ctx = EnsoContext.get(this);
    var err = ctx.getBuiltins().error().makeTypeError("polyglot array", arr, "array");
    throw new PanicException(err, this);
  }
}
