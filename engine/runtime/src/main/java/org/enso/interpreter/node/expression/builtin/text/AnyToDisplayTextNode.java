package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Any", name = "to_display_text")
public abstract class AnyToDisplayTextNode extends Node {
  static AnyToDisplayTextNode build() {
    return AnyToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object self);

  @Specialization(guards = {"displays.isException(self)", "displays.hasExceptionMessage(self)"})
  Text showExceptions(
      Object self,
      @CachedLibrary(limit = "3") InteropLibrary strings,
      @CachedLibrary(limit = "3") InteropLibrary displays) {
    try {
      return Text.create(strings.asString(displays.getExceptionMessage(self)));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Fallback
  Text doShowType(Object self, @Cached TypeToDisplayTextNode typeToDisplayTextNode) {
    return Text.create(typeToDisplayTextNode.execute(self));
  }
}
