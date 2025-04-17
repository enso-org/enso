package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Meta",
    name = "get_constructor_fields",
    description = "Gets the field names of a constructor.",
    autoRegister = false)
public abstract class GetConstructorFieldNamesNode extends Node {
  static GetConstructorFieldNamesNode build() {
    return GetConstructorFieldNamesNodeGen.create();
  }

  abstract Object execute(Object obj);

  @Specialization
  final Object fieldNamesForAtomCtor(AtomConstructor atomConstructor) {
    if (atomConstructor.getType().hasAllConstructorsPrivate()) {
      var ctx = EnsoContext.get(this);
      var err = ctx.getBuiltins().error().makePrivateAccessError("", "", "");
      return DataflowError.withDefaultTrace(err, this);
    }
    ArgumentDefinition[] fields = atomConstructor.getFields();
    Text[] result = new Text[fields.length];
    for (int i = 0; i < fields.length; i++) {
      result[i] = Text.create(fields[i].getName());
    }
    return ArrayLikeHelpers.asVectorEnsoObjects(result);
  }

  @Fallback
  final EnsoObject fieldNamesForAny(Object any) {
    return ArrayLikeHelpers.asVectorEmpty();
  }
}
