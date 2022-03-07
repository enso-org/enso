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

  abstract Text execute(Object _this);

  @Specialization
  Text doAtom(Atom _this) {
    Object[] fields = _this.getFields();

    Text expected;
    if (fields[0].equals(fields[1])) {
        expected = Text.create(String.valueOf(fields[0]));
    } else {
        expected = Text.create(String.valueOf(fields[0]))
            .add("-")
            .add(String.valueOf(fields[1]));
    }

    return Text.create("Wrong number of arguments. Expected ")
        .add(expected)
        .add(", but got ")
        .add(String.valueOf(fields[2]))
        .add(".");
  }

  @Specialization
  Text doConstructor(AtomConstructor _this) {
    return Text.create("Wrong number of arguments.");
  }
}
