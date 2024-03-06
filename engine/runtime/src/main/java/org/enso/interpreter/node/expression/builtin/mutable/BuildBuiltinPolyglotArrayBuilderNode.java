package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot_Array_Builder",
    name = "build_builtin",
    description = "Creates a Vector from this Polyglot_Array_Builder.",
    autoRegister = true)
public abstract class BuildBuiltinPolyglotArrayBuilderNode extends Node {

  static BuildBuiltinPolyglotArrayBuilderNode build() {
    return BuildBuiltinPolyglotArrayBuilderNodeGen.create();
  }

  abstract EnsoObject execute(Object arr);

  @Specialization
  EnsoObject doArrowArrayBuilder(
      org.enso.interpreter.runtime.data.vector.PolyglotArrayBuilder builder) {
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
