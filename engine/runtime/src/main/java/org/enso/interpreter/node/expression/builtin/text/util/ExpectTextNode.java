package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@ReportPolymorphism
public abstract class ExpectTextNode extends Node {
  private @Child InteropLibrary library = InteropLibrary.getFactory().createDispatched(10);

  public abstract Text execute(Object o);

  public static ExpectTextNode build() {
    return ExpectTextNodeGen.create();
  }

  @Specialization
  Text doText(Text o) {
    return o;
  }

  @Specialization
  Text doString(String o) {
    return Text.create(o);
  }

  @Fallback
  Text doFallback(Object o) {
    try {
      return Text.create(library.asString(o));
    } catch (UnsupportedMessageException e) {
      Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
      Atom err = builtins.error().makeTypeError(builtins.text().getText(), o);
      throw new PanicException(err, this);
    }
  }
}
