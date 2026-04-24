package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.polyglot.common_utils.Core_Text_Utils;

@BuiltinMethod(type = "Any", name = Constants.Names.TO_DISPLAY_TEXT)
public abstract class AnyToDisplayTextNode extends Node {
  static AnyToDisplayTextNode build() {
    return AnyToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object obj);

  @Specialization(guards = {"iop.isException(obj)", "iop.hasExceptionMessage(obj)"})
  Text showExceptions(Object obj, @Shared("iop") @CachedLibrary(limit = "3") InteropLibrary iop) {
    try {
      return Text.create(iop.asString(iop.getExceptionMessage(obj)));
    } catch (UnsupportedMessageException e) {
      throw EnsoContext.get(iop).raiseAssertionPanic(iop, null, e);
    }
  }

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text convertInteger(long obj) {
    return Text.create(Long.toString(obj));
  }

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Text convertDouble(double obj) {
    return Text.create(Double.toString(obj));
  }

  @Specialization
  Text convertBigInteger(EnsoBigInteger bigInteger) {
    return Text.create(bigInteger.toString());
  }

  @Specialization
  Text convertText(Text obj) {
    final var limit = 80;
    if (Text.length(obj) < limit) {
      return obj;
    } else {
      return takePrefix(obj, limit);
    }
  }

  @Specialization
  Text convertBoolean(boolean obj) {
    return Text.create(obj ? "True" : "False");
  }

  @Specialization
  Text convertAtomConstructor(AtomConstructor obj) {
    return Text.create(obj.getDisplayName());
  }

  @Specialization
  Text convertAtom(Atom obj) {
    return convertAtomConstructor(obj.getConstructor());
  }

  @Specialization
  Text convertType(Type obj) {
    return Text.create(obj.getName());
  }

  @Specialization(
      guards = {"iop.isMetaObject(obj)"},
      rewriteOn = UnsupportedMessageException.class)
  Text convertMetaObject(Object obj, @Shared("iop") @CachedLibrary(limit = "3") InteropLibrary iop)
      throws UnsupportedMessageException {
    var maybeName = iop.getMetaQualifiedName(obj);
    var name = iop.asString(maybeName);
    return Text.create(name);
  }

  @CompilerDirectives.TruffleBoundary
  private static Text takePrefix(Text obj, final int limit) {
    var prefix = Core_Text_Utils.take_prefix(obj.toString(), limit - 2);
    return Text.create(prefix + " …");
  }

  @Fallback
  Text doShowType(Object obj, @Cached TypeToDisplayTextNode typeToDisplayTextNode) {
    return Text.create(typeToDisplayTextNode.execute(obj));
  }
}
