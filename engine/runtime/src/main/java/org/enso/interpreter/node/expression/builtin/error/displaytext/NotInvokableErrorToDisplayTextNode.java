package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Not_Invokable_Error", name = "to_display_text")
public abstract class NotInvokableErrorToDisplayTextNode extends Node {
  static NotInvokableErrorToDisplayTextNode build() {
    return NotInvokableErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object _this);

  @Specialization
  Text doAtom(Atom _this, @Cached TypeToDisplayTextNode displayTypeNode) {
    return Text.create("Type error: expected a function, but got ")
        .add(displayTypeNode.execute(_this.getFields()[0]))
        .add(".");
  }

  @Specialization
  Text doConstructor(AtomConstructor _this) {
    return Text.create("Type error.");
  }
}
