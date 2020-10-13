package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_constructor_fields",
    description = "Gets the field names of a constructor.")
public class GetConstructorFieldNamesNode extends Node {
  Array execute(Object _this, AtomConstructor atom_constructor) {
    ArgumentDefinition[] fields = atom_constructor.getFields();
    Object[] result = new Object[fields.length];
    for (int i = 0; i < fields.length; i++) {
      result[i] = Text.create(fields[i].getName());
    }
    return new Array(result);
  }
}
