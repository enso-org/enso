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
public abstract class IsTextNode extends Node {
  private @Child InteropLibrary library = InteropLibrary.getFactory().createDispatched(10);

  public abstract boolean execute(Object o);

  @Specialization
  boolean doText(Text o) {
    return true;
  }

  @Specialization
  boolean doString(String o) {
    return true;
  }

  @Fallback
  boolean doFallback(Object o) {
    return library.isString(o);
  }
}
