package org.enso.interpreter.node.expression.builtin.interop.syntax;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Any",
    name = "<to_text>",
    description = "Returns human-readable representation of a polyglot object.")
public class PolyglotToTextNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child InteropLibrary strings =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  public Text execute(Object _this) {
    try {
      return Text.create(strings.asString(library.toDisplayString(_this)));
    } catch (UnsupportedMessageException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
