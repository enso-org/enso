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
    return Text.create("Wrong number of arguments. Expected ")
        .add(String.valueOf(_this.getFields()[0]))
        .add(", but got ")
        .add(String.valueOf(_this.getFields()[1]))
        .add(".");
  }

  @Specialization
  Text doConstructor(AtomConstructor _this) {
    return Text.create("Wrong number of arguments.");
  }
}
