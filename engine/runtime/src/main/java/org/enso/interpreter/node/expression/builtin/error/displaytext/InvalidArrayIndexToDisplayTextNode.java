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

@BuiltinMethod(type = "Invalid_Array_Index", name = "to_display_text")
public abstract class InvalidArrayIndexToDisplayTextNode extends Node {
  static InvalidArrayIndexToDisplayTextNode build() {
    return InvalidArrayIndexToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text doAtom(Atom self, @CachedLibrary(limit = "3") StructsLibrary structs) {
    return Text.create("Invalid array index: ", String.valueOf(structs.getField(self, 1)));
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Invalid array index.");
  }
}
