package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_constructor_name",
    description = "Gets the name of a constructor.",
    autoRegister = false)
public abstract class GetConstructorNameNode extends Node {
  static GetConstructorNameNode build() {
    return GetConstructorNameNodeGen.create();
  }

  abstract Text execute(Object atom_constructor);

  @Specialization
  Text doConstructor(AtomConstructor cons) {
    return Text.create(cons.getName());
  }

  @Specialization
  Text doType(Type type) {
    return Text.create(type.getQualifiedName().toString());
  }
}
