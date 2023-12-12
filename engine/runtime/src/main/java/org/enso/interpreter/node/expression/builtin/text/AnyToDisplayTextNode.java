package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.polyglot.common_utils.Core_Text_Utils;

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
      throw EnsoContext.get(strings).raiseAssertionPanic(strings, null, e);
    }
  }

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text convertInteger(long self) {
    return Text.create(Long.toString(self));
  }

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text convertDouble(double self) {
    return Text.create(Double.toString(self));
  }

  @Specialization
  Text convertBigInteger(EnsoBigInteger bigInteger) {
    return Text.create(bigInteger.toString());
  }

  @Specialization
  Text convertText(Text self) {
    final var limit = 80;
    if (self.length() < limit) {
      return self;
    } else {
      return takePrefix(self, limit);
    }
  }

  @Specialization
  Text convertBoolean(boolean self) {
    return Text.create(self ? "True" : "False");
  }

  @Specialization
  Text convertAtomConstructor(AtomConstructor self) {
    return Text.create(self.getDisplayName());
  }

  @Specialization
  Text convertAtom(Atom self) {
    return convertAtomConstructor(self.getConstructor());
  }

  @Specialization
  Text convertType(Type self) {
    return Text.create(self.getName());
  }

  @CompilerDirectives.TruffleBoundary
  private static Text takePrefix(Text self, final int limit) {
    var prefix = Core_Text_Utils.take_prefix(self.toString(), limit - 2);
    return Text.create(prefix + " …");
  }

  @Fallback
  Text doShowType(Object self, @Cached TypeToDisplayTextNode typeToDisplayTextNode) {
    return Text.create(typeToDisplayTextNode.execute(self));
  }
}
