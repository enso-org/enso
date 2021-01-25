package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(type = "Error", name = "to_text", description = "Convert an error to text.")
public class ErrorToTextNode extends Node {
  private static final int DISPATCH_CACHE = 3;
  private @Child InteropLibrary displays =
      InteropLibrary.getFactory().createDispatched(DISPATCH_CACHE);
  private @Child InteropLibrary strings =
      InteropLibrary.getFactory().createDispatched(DISPATCH_CACHE);

  public Text execute(DataflowError _this) {
    try {
      return Text.create(strings.asString(displays.toDisplayString(_this)));
    } catch (UnsupportedMessageException ignored) {
      throw new IllegalStateException("Unreachable");
    }
  }
}
