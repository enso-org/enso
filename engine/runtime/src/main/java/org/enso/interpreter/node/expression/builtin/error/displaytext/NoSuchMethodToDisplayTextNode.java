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

@BuiltinMethod(type = "No_Such_Method", name = "to_display_text")
public abstract class NoSuchMethodToDisplayTextNode extends Node {
  static NoSuchMethodToDisplayTextNode build() {
    return NoSuchMethodToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(Atom self, @Cached TypeToDisplayTextNode displayTypeNode) {
    try {
      return Text.create("Method `")
          .add(TypesGen.expectUnresolvedSymbol(self.getFields()[1]).getName())
          .add("` of ")
          .add(displayTypeNode.execute(self.getFields()[0]))
          .add(" could not be found.");
    } catch (UnexpectedResultException e) {
      return Text.create("Method could not be found.");
    }
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Method could not be found.");
  }
}
