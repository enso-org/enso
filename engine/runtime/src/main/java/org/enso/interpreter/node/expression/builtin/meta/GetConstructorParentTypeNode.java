package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_constructor_parent_type",
    description = "Gets the name of a constructor.",
    autoRegister = false)
public abstract class GetConstructorParentTypeNode extends Node {
  static GetConstructorParentTypeNode build() {
    return GetConstructorParentTypeNodeGen.create();
  }

  Type execute(AtomConstructor cons) {
    return cons.getType();
  }
}
