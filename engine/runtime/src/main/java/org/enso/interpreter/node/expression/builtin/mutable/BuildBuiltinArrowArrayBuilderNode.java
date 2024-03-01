package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Arrow_Array_Builder",
    name = "build_builtin",
    description = "Creates a Vector from this Arrow_Array_Builder.",
    autoRegister = true)
public abstract class BuildBuiltinArrowArrayBuilderNode extends Node {

  static BuildBuiltinArrowArrayBuilderNode build() {
    return BuildBuiltinArrowArrayBuilderNodeGen.create();
  }

  abstract EnsoObject execute(Object arr);

  @Specialization
  EnsoObject doArrowArrayBuilder(
      org.enso.interpreter.runtime.data.vector.ArrowArrayBuilder builder) {
    builder.seal();
    return org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers.asVectorFromArray(builder);
  }

  @Fallback
  EnsoObject doOther(Object arr) {
    var ctx = EnsoContext.get(this);
    throw new PanicException(
        ctx.getBuiltins().error().makeTypeError("polyglot array", arr, "array"), this);
  }
}
