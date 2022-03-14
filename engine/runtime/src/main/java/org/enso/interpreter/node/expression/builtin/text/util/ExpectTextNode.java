package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@ReportPolymorphism
public abstract class ExpectTextNode extends Node {
  private @Child InteropLibrary library = InteropLibrary.getFactory().createDispatched(10);

  public abstract Text execute(Object str);

  public static ExpectTextNode build() {
    return ExpectTextNodeGen.create();
  }

  @Specialization
  Text doText(Text str) {
    return str;
  }

  @Specialization
  Text doString(String str) {
    return Text.create(str);
  }

  @Fallback
  Text doFallback(Object str) {
    try {
      return Text.create(library.asString(str));
    } catch (UnsupportedMessageException e) {
      Builtins builtins = Context.get(this).getBuiltins();
      Atom err = builtins.error().makeTypeError(builtins.text().getText(), str, "str");
      throw new PanicException(err, this);
    }
  }
}
