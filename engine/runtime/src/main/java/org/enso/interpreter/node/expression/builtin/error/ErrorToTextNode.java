package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(type = "Error", name = "to_text", description = "Convert an error to text.")
public abstract class ErrorToTextNode extends Node {
  private static final int DISPATCH_CACHE = 3;
  private @Child InteropLibrary displays =
      InteropLibrary.getFactory().createDispatched(DISPATCH_CACHE);
  private @Child InteropLibrary strings =
      InteropLibrary.getFactory().createDispatched(DISPATCH_CACHE);

  static ErrorToTextNode build() {
    return ErrorToTextNodeGen.create();
  }

  public abstract Text execute(@AcceptsError Object self);

  @Specialization
  public Text doDataflowError(DataflowError self) {
    try {
      return Text.create(strings.asString(displays.toDisplayString(self)));
    } catch (UnsupportedMessageException ignored) {
      throw new IllegalStateException("Unreachable");
    }
  }

  @Specialization
  public Text doAtom(Atom self) {
    return Text.create("Error");
  }
}
