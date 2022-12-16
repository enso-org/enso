package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;

public abstract class ExpectStringNode extends Node {
  private @Child InteropLibrary library = InteropLibrary.getFactory().createDispatched(10);

  public abstract String execute(Object str);

  public static ExpectStringNode build() {
    return ExpectStringNodeGen.create();
  }

  @Specialization
  String doText(Text o, @Cached("build()") ToJavaStringNode toJavaStringNode) {
    return toJavaStringNode.execute(o);
  }

  @Specialization
  String doString(String str) {
    return str;
  }

  @Specialization(guards = "warnings.hasWarnings(warning)")
  String doWarning(Object warning, @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    try {
      return execute(warnings.removeWarnings(warning));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Fallback
  String doFallback(Object str) {
    try {
      return library.asString(str);
    } catch (UnsupportedMessageException e) {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      Atom err = builtins.error().makeTypeError(builtins.text(), str, "str");
      throw new PanicException(err, this);
    }
  }
}
