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
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "get_type_methods",
    description = "Gets the method names of a type.",
    autoRegister = false)
public abstract class GetTypeMethodsNode extends Node {
  static GetTypeMethodsNode build() {
    return GetTypeMethodsNodeGen.create();
  }

  abstract EnsoObject execute(Object type);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  EnsoObject allMethods(Type type) {
    var methods = type.getDefinitionScope().getMethodNamesForType(type);
    return methods == null
        ? ArrayLikeHelpers.empty()
        : ArrayLikeHelpers.wrapStrings(methods.toArray(new String[0]));
  }

  @Fallback
  @CompilerDirectives.TruffleBoundary
  EnsoObject empty(Object type) {
    var ctx = EnsoContext.get(this);
    var builtins = ctx.getBuiltins();
    Atom payload = builtins.error().makeTypeError("Type", type, "type");
    throw new PanicException(payload, this);
  }
}
