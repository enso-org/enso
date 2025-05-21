package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

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
    var withCheck = FindAtomConstructorNode.findAtomConstructor(this, atomConstructor, null);
    if (withCheck == atomConstructor) {
      var fields = atomConstructor.getFields();
      var result = new Text[fields.length];
      for (int i = 0; i < fields.length; i++) {
        result[i] = Text.create(fields[i].getName());
      }
      return ArrayLikeHelpers.asVectorEnsoObjects(result);
    } else {
      return withCheck;
    }
  }

  @Fallback
  final EnsoObject fieldNamesForAny(Object any) {
    return ArrayLikeHelpers.asVectorEmpty();
  }
}
