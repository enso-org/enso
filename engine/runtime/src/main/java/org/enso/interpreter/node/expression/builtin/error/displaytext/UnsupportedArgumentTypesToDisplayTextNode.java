package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;

import java.util.Arrays;
import java.util.stream.Collectors;

@BuiltinMethod(type = "Unsupported_Argument_Types", name = "to_display_text")
public abstract class UnsupportedArgumentTypesToDisplayTextNode extends Node {
  static UnsupportedArgumentTypesToDisplayTextNode build() {
    return UnsupportedArgumentTypesToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text doAtom(Atom self, @Cached TypeToDisplayTextNode displayTypeNode) {
    Object args = self.getFields()[0];
    String argsRep;
    if (args instanceof Array) {
      Object[] arguments = ((Array) args).getItems();
      argsRep =
          Arrays.stream(arguments)
              .map(displayTypeNode::execute)
              .collect(Collectors.joining(", ", "[", "]"));
    } else {
      argsRep = displayTypeNode.execute(args);
    }
    return Text.create("Unsupported argument types: ").add(argsRep);
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Unsupported argument types.");
  }
}
