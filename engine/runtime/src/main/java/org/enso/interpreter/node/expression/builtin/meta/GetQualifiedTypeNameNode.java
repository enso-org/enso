package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_qualified_type_name",
    description = "Returns a qualified type name of the given value.",
    autoRegister = false)
public class GetQualifiedTypeNameNode extends Node {
  private @Child TypeOfNode typeOfNode = TypeOfNode.build();

  Object execute(@AcceptsError Object value) {
    var maybeType =
        switch (value) {
          case Type type -> type;
          default -> typeOfNode.execute(value);
        };
    if (maybeType instanceof Type type) {
      return Text.create(type.getQualifiedName().toString());
    }
    return EnsoContext.get(this).getBuiltins().nothing();
  }
}
