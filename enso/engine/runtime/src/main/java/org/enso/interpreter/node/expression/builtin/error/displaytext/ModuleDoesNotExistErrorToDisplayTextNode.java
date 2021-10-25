package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "Module_Does_Not_Exist_Error", name = "to_display_text")
public abstract class ModuleDoesNotExistErrorToDisplayTextNode extends Node {
  static ModuleDoesNotExistErrorToDisplayTextNode build() {
    return ModuleDoesNotExistErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object _this);

  @Specialization
  Text doAtom(Atom _this) {
    try {
      return Text.create("Module ")
          .add(TypesGen.expectText(_this.getFields()[0]))
          .add(" does not exist.");
    } catch (UnexpectedResultException e) {
      return Text.create("Module does not exist.");
    }
  }

  @Specialization
  Text doConstructor(AtomConstructor _this) {
    return Text.create("Module does not exist.");
  }
}
