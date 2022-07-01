package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Arity_Error", name = "to_display_text")
public abstract class ArityErrorToDisplayTextNode extends Node {
  static ArityErrorToDisplayTextNode build() {
    return ArityErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(Atom self) {
    Object[] fields = self.getFields();

    Text expected = Text.create(String.valueOf(fields[0]));
    if (!fields[0].equals(fields[1])) {
      expected = expected.add("-");
      if (!fields[1].equals(-1)) {
        expected = expected.add(String.valueOf(fields[1]));
      }
    }

    return Text.create("Wrong number of arguments. Expected ")
        .add(expected)
        .add(", but got ")
        .add(String.valueOf(fields[2]))
        .add(".");
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Wrong number of arguments.");
  }
}
