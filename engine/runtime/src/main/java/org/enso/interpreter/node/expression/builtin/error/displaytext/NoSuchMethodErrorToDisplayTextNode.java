package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.data.struct.Struct;
import org.enso.interpreter.runtime.data.struct.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "No_Such_Method_Error", name = "to_display_text")
public abstract class NoSuchMethodErrorToDisplayTextNode extends Node {
  static NoSuchMethodErrorToDisplayTextNode build() {
    return NoSuchMethodErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(Struct self, @Cached TypeToDisplayTextNode displayTypeNode) {
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
