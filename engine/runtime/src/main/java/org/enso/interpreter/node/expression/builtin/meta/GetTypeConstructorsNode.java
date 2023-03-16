package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;

@BuiltinMethod(
    type = "Meta",
    name = "get_type_constructors",
    description = "Gets the constructors of a type.",
    autoRegister = false)
public abstract class GetTypeConstructorsNode extends Node {
  static GetTypeConstructorsNode build() {
    return GetTypeConstructorsNodeGen.create();
  }

  abstract Array execute(Object type, Object factory);

  @Specialization
  Array allConstructors(Type type, AtomConstructor factory) {
    var rawConstructors = type.getConstructors().values();
    var rawResult = new Object[rawConstructors.size()];
    int at = 0;
    for (var cons : rawConstructors) {
      var metaCons = factory.newInstance(cons);
      rawResult[at++] = metaCons;
    }
    return new Array(rawResult);
  }

  @Fallback
  Array empty(Object type, Object any) {
    return Array.empty();
  }
}
