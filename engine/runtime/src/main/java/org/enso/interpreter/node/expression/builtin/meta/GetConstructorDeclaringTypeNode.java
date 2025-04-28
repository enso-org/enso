package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

@BuiltinMethod(
    type = "Meta",
    name = "get_constructor_declaring_type",
    description = "Gets the type that declared this constructor.",
    autoRegister = false)
public class GetConstructorDeclaringTypeNode extends Node {
  Object execute(AtomConstructor cons) {
    var withCheck = FindAtomConstructorNode.findAtomConstructor(this, cons, null);
    if (withCheck == cons) {
      return cons.getType();
    } else {
      return withCheck;
    }
  }
}
