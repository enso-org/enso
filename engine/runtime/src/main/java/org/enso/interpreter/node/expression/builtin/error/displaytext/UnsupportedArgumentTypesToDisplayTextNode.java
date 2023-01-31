package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Unsupported_Argument_Types", name = "to_display_text")
public abstract class UnsupportedArgumentTypesToDisplayTextNode extends Node {
  static UnsupportedArgumentTypesToDisplayTextNode build() {
    return UnsupportedArgumentTypesToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(
      Atom self,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @CachedLibrary(limit = "3") StructsLibrary structs) {
    Object messageArg = structs.getField(self, 1);
    try {
      return Text.create(interop.asString(messageArg));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Unsupported argument types.");
  }
}
