package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;

@BuiltinMethod(
    type = "Meta",
    name = "get_qualified_type_name",
    description = "Returns a qualified type name of the given value.",
    autoRegister = false)
public class GetQualifiedTypeNameNode extends Node {
  private @Child TypeOfNode typeOfNode = TypeOfNode.create();

  Object execute(@AcceptsError Object value) {
    var maybeType =
        switch (value) {
          case Type type -> type;
          default -> typeOfNode.execute(value);
        };
    if (maybeType instanceof Type type) {
      return Text.create(getQualifiedName(type));
    }
    return EnsoContext.get(this).getBuiltins().nothing();
  }

  @TruffleBoundary
  private static String getQualifiedName(Type type) {
    return type.getQualifiedName().toString();
  }
}
