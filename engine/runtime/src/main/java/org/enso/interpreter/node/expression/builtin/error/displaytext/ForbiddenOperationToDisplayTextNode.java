package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Forbidden_Operation", name = "to_display_text")
public abstract class ForbiddenOperationToDisplayTextNode extends Node {
  static ForbiddenOperationToDisplayTextNode build() {
    return ForbiddenOperationToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text doAtom(Atom self, @CachedLibrary(limit = "3") StructsLibrary structs) {
    return Text.create("Forbidden operation: ")
        .add(String.valueOf(structs.getField(self, 0)))
        .add(".");
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Forbidden operation.");
  }
}
