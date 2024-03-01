package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Arrow_Array_Builder",
    name = "from_arrow_array_builtin",
    description = "Creates an Arrow_Array_Builder from a polyglot array.",
    autoRegister = true)
public abstract class FromArrowArrayBuiltinArrowArrayBuilderNode extends Node {

  static FromArrowArrayBuiltinArrowArrayBuilderNode build() {
    return FromArrowArrayBuiltinArrowArrayBuilderNodeGen.create();
  }

  abstract EnsoObject execute(Object arr);

  @Specialization(guards = "interop.hasArrayElements(arr)")
  EnsoObject doObject(Object arr, @CachedLibrary(limit = "1") InteropLibrary interop) {
    return org.enso.interpreter.runtime.data.vector.ArrowArrayBuilder.fromArrowArray(arr);
  }

  @Fallback
  EnsoObject doOther(Object arr) {
    var ctx = EnsoContext.get(this);
    throw new PanicException(
        ctx.getBuiltins().error().makeTypeError("polyglot array", arr, "array"), this);
  }
}
