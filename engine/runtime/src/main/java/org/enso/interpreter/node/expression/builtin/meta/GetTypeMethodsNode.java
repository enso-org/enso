package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;
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

  abstract Array execute(Object type);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Array allMethods(Type type) {
    var methodNames = type.getDefinitionScope().getMethods().get(type).keySet();
    return new Array(methodNames.toArray());
  }

  @Fallback
  @CompilerDirectives.TruffleBoundary
  Array empty(Object type) {
    var ctx = EnsoContext.get(this);
    var builtins = ctx.getBuiltins();
    Atom payload = builtins.error().makeTypeError("Type", type, "type");
    throw new PanicException(payload, this);
  }
}
