package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

@BuiltinMethod(
    type = "Meta",
    name = "get_multi_types",
    description =
        "Returns all types that the current value pretends to be. The first returned array contains"
            + " all the visible types and the second one contains the hidden types that can be"
            + " extracted via casting.",
    autoRegister = false)
final class GetMultiTypesBuiltin extends Node {
  private @Child WarningsLibrary warnings = WarningsLibrary.getFactory().createDispatched(11);

  private @Child TypeOfNode typeOf = TypeOfNode.create();

  GetMultiTypesBuiltin() {}

  public EnsoObject execute(@AcceptsWarning Object value) {
    if (warnings.hasWarnings(value)) {
      try {
        value = warnings.removeWarnings(value);
      } catch (UnsupportedMessageException ex) {
        var ctx = EnsoContext.get(this);
        throw ctx.raiseAssertionPanic(this, null, ex);
      }
    }

    if (value instanceof EnsoMultiValue multiValue) {
      return makeReturnTypes(multiValue.getVisibleTypes(), multiValue.getExtraTypes());
    } else {
      var singleType = typeOf.findTypeOrNull(value);
      if (singleType == null) {
        return makeReturnTypes(new Type[0], new Type[0]);
      }
      return makeReturnTypes(new Type[] {singleType}, new Type[0]);
    }
  }

  private EnsoObject makeReturnTypes(Type[] visibleTypes, Type[] extraTypes) {
    return ArrayLikeHelpers.asVectorEnsoObjects(
        ArrayLikeHelpers.asVectorEnsoObjects(visibleTypes),
        ArrayLikeHelpers.asVectorEnsoObjects(extraTypes));
  }
}
