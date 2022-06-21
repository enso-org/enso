package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "get_executable_name",
    description = "Returns the executable name of a polyglot object.")
public class GetExecutableNameNode extends Node {
  private @Child InteropLibrary functionsLibrary =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child InteropLibrary stringsLibrary =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Text execute(Object self, Object function) {
    try {
      return Text.create(stringsLibrary.asString(functionsLibrary.getExecutableName(function)));
    } catch (UnsupportedMessageException e) {
      err.enter();
      Builtins builtins = Context.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError(builtins.function(), function, "function"), this);
    }
  }
}
