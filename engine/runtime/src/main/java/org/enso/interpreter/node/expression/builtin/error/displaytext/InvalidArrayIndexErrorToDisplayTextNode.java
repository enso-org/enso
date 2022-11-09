package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.struct.Struct;
import org.enso.interpreter.runtime.data.struct.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Invalid_Array_Index_Error", name = "to_display_text")
public abstract class InvalidArrayIndexErrorToDisplayTextNode extends Node {
  static InvalidArrayIndexErrorToDisplayTextNode build() {
    return InvalidArrayIndexErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text doAtom(Struct self) {
    return Text.create("Invalid array index: ", String.valueOf(self.getFields()[1]));
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Invalid array index.");
  }
}
