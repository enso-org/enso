package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.type.Types;

@BuiltinMethod(
    type = "Meta",
    name = "get_qualified_type_name",
    description = "Returns a qualified type name of the given value.")
public class GetQualifiedTypeNameNode extends Node {
  Text execute(@AcceptsError Object self, Object value) {
    var typeName = Types.getName(value);
    return Text.create(typeName);
  }
}
