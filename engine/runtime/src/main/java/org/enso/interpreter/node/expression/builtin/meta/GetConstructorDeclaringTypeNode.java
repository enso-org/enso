package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;

@BuiltinMethod(
    type = "Meta",
    name = "get_constructor_declaring_type",
    description = "Gets the type that declared this constructor.",
    autoRegister = false)
public class GetConstructorDeclaringTypeNode extends Node {
  Type execute(AtomConstructor cons) {
    return cons.getType();
  }
}
