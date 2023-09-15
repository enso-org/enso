package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_short_type_name",
    description = "Gets the short name of a Type.",
    autoRegister = false)
public class GetShortTypeNameNode extends Node {
  Text execute(Type type) {
    return Text.create(type.getName());
  }
}
