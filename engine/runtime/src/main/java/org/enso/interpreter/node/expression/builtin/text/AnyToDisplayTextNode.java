package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Any", name = "to_display_text")
public abstract class AnyToDisplayTextNode extends Node {
  static AnyToDisplayTextNode build() {
    return AnyToDisplayTextNodeGen.create();
  }

  abstract Text execute(Object _this);

  @Specialization
  Text doShowType(Object _this, @Cached TypeToDisplayTextNode typeToDisplayTextNode) {
    return Text.create(typeToDisplayTextNode.execute(_this));
  }
}
