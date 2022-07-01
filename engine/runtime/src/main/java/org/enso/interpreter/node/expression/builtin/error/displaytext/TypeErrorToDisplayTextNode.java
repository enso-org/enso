package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "Type_Error", name = "to_display_text")
public abstract class TypeErrorToDisplayTextNode extends Node {
  static TypeErrorToDisplayTextNode build() {
    return TypeErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(Atom self, @Cached TypeToDisplayTextNode displayTypeNode) {
    try {
      return Text.create("Type error: expected `")
          .add(TypesGen.expectText(self.getFields()[2]))
          .add("` to be ")
          .add(displayTypeNode.execute(self.getFields()[0]))
          .add(", but got ")
          .add(displayTypeNode.execute(self.getFields()[1]))
          .add(".");
    } catch (UnexpectedResultException e) {
      return Text.create("Type error.");
    }
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Type error.");
  }
}
