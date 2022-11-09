package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.data.struct.Struct;
import org.enso.interpreter.runtime.data.struct.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Not_Invokable_Error", name = "to_display_text")
public abstract class NotInvokableErrorToDisplayTextNode extends Node {
  static NotInvokableErrorToDisplayTextNode build() {
    return NotInvokableErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(Struct self, @Cached TypeToDisplayTextNode displayTypeNode) {
    return Text.create("Type error: expected a function, but got ")
        .add(displayTypeNode.execute(self.getFields()[0]))
        .add(".");
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Type error.");
  }
}
