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
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "get_type_methods",
    description = "Gets the method names of a type.",
    autoRegister = false)
public abstract class GetTypeMethodsNode extends Node {
  public static GetTypeMethodsNode build() {
    return GetTypeMethodsNodeGen.create();
  }

  public abstract EnsoObject execute(Object type);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  final EnsoObject allMethods(Type type) {
    var methods = type.getMethods(true);
    var methodNames =
        methods.entrySet().stream()
            .filter(e -> !e.getValue().getSchema().isProjectPrivate())
            .map(e -> Text.create(e.getKey()))
            .toArray(Text[]::new);
    return ArrayLikeHelpers.asVectorEnsoObjects(methodNames);
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
