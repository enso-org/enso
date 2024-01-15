package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
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

  abstract EnsoObject execute(Object type, Object factory);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  EnsoObject allConstructors(Type type, AtomConstructor factory) {
    var rawConstructors = type.getConstructors().values();
    var rawResult = new EnsoObject[rawConstructors.size()];
    int at = 0;
    for (var cons : rawConstructors) {
      var metaCons = AtomNewInstanceNode.getUncached().newInstance(factory, cons);
      rawResult[at++] = metaCons;
    }
    return ArrayLikeHelpers.wrapEnsoObjects(rawResult);
  }

  @Fallback
  @CompilerDirectives.TruffleBoundary
  EnsoObject empty(Object type, Object factory) {
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
