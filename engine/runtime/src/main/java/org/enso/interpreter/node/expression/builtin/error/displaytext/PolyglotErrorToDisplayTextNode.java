package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "Polyglot_Error", name = "to_display_text")
public abstract class PolyglotErrorToDisplayTextNode extends Node {
  static PolyglotErrorToDisplayTextNode build() {
    return PolyglotErrorToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object _this);

  @Specialization
  Text doAtom(
      Atom _this,
      @Cached TypeToDisplayTextNode displayTypeNode,
      @CachedLibrary(limit = "5") InteropLibrary exceptions,
      @CachedLibrary(limit = "5") InteropLibrary strings) {
    try {
      Object cause = _this.getFields()[0];
      String rep;
      if (exceptions.hasExceptionMessage(cause)) {
        rep = strings.asString(exceptions.getExceptionCause(cause));
      } else {
        rep = displayTypeNode.execute(cause);
      }
      return Text.create("Polyglot error: ").add(rep);
    } catch (UnsupportedMessageException e) {
      return Text.create("Polyglot error.");
    }
  }

  @Specialization
  Text doConstructor(AtomConstructor _this) {
    return Text.create("Polyglot error.");
  }
}
