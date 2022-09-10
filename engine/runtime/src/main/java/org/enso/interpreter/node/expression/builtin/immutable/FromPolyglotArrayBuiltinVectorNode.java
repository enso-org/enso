package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.*;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(
    type = "Vector",
    name = "from_polyglot_array",
    description = "Returns an Array representation of this Vector.")
public abstract class FromPolyglotArrayBuiltinVectorNode extends Node {

  static FromPolyglotArrayBuiltinVectorNode build() {
    return FromPolyglotArrayBuiltinVectorNodeGen.create();
  }

  abstract Vector execute(Object arr);

  @Specialization(guards = "interop.hasArrayElements(arr)")
  Vector doObject(Object arr, @CachedLibrary(limit = "1") InteropLibrary interop) {
    return Vector.fromArray(arr);
  }

  @Fallback
  Vector doOther(Object arr) {
    Context ctx = Context.get(this);
    throw new PanicException(
        ctx.getBuiltins().error().makeTypeError("polyglot array", arr, "array"), this);
  }
}
