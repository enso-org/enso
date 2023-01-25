package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "Module_Does_Not_Exist", name = "to_display_text")
public abstract class ModuleDoesNotExistToDisplayTextNode extends Node {
  static ModuleDoesNotExistToDisplayTextNode build() {
    return ModuleDoesNotExistToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(Atom self, @CachedLibrary(limit = "3") StructsLibrary structs) {
    try {
      return Text.create("Module ")
          .add(TypesGen.expectText(structs.getField(self, 0)))
          .add(" does not exist.");
    } catch (UnexpectedResultException e) {
      return Text.create("Module does not exist.");
    }
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Module does not exist.");
  }
}
