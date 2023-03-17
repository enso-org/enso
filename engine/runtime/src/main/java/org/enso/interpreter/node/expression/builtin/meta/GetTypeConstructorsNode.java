package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.type.TypesGen;

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
  @CompilerDirectives.TruffleBoundary
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
  @CompilerDirectives.TruffleBoundary
  Array empty(Object type, Object factory) {
    var ctx = EnsoContext.get(this);
    var builtins = ctx.getBuiltins();
    Atom payload;
    if (TypesGen.isType(type)) {
      payload = builtins.error().makeTypeError("AtomConstructor", factory, "factory");
    } else {
      payload = builtins.error().makeTypeError("Type", type, "type");
    }
    throw new PanicException(payload, this);
  }
}
