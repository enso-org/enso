package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Meta", name = "get_simple_type_name", description = "Pretty prints a type.")
public class GetSimpleTypeNameNode extends Node {
  @Child @CompilationFinal TypeToDisplayTextNode displayTypeNode = TypeToDisplayTextNode.build();

  Text execute(@AcceptsError Object value) {
    return Text.create(displayTypeNode.execute(value));
  }
}
