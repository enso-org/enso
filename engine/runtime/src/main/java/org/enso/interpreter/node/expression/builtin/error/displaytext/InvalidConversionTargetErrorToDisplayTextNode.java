package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Invalid_Conversion_Target_Error", name = "to_display_text")
public abstract class InvalidConversionTargetErrorToDisplayTextNode extends Node {
  static InvalidConversionTargetErrorToDisplayTextNode build() {
    return InvalidConversionTargetErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization
  Text doAtom(
      Atom self,
      @CachedLibrary(limit = "10") InteropLibrary interopLibrary,
      @Cached TypeToDisplayTextNode fallback) {
    String fieldRep;
    Object target = self.getFields()[0];
    try {
      fieldRep = interopLibrary.asString(interopLibrary.toDisplayString(target));
    } catch (UnsupportedMessageException e) {
      fieldRep = fallback.execute(target);
    }
    return Text.create(fieldRep).add(" is not a valid conversion target. Expected a type.");
  }

  @Specialization
  Text doConstructor(AtomConstructor self) {
    return Text.create("Invalid conversion target type.");
  }
}
