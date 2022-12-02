package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(
    type = "Vector",
    name = "from_array",
    description = "Creates a Vector by copying Array content.",
    autoRegister = false)
public abstract class FromArrayBuiltinVectorNode extends Node {
  static FromArrayBuiltinVectorNode build() {
    return FromArrayBuiltinVectorNodeGen.create();
  }

  abstract Vector execute(Object arr);

  @Specialization
  Vector fromVector(Vector arr) {
    return arr;
  }

  @Specialization(guards = "interop.hasArrayElements(arr)")
  Vector fromArrayLikeObject(
      Object arr,
      @Cached CoerceArrayNode coerce,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return Vector.fromArray(new Array(coerce.execute(arr)));
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
