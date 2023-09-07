package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "get_executable_name",
    description = "Returns the executable name of a polyglot object.",
    autoRegister = false)
public class GetExecutableNameNode extends Node {
  private @Child InteropLibrary functionsLibrary =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child InteropLibrary stringsLibrary =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  Text execute(Object function) {
    // Workaround for https://github.com/oracle/graal/issues/7359
    if (!functionsLibrary.hasExecutableName(function)) {
      err.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError(builtins.function(), function, "function"), this);
    }

    try {
      var name = functionsLibrary.getExecutableName(function);
      if (name == null || !stringsLibrary.isString(name)) {
        CompilerDirectives.transferToInterpreter();
        throw CompilerDirectives.shouldNotReachHere("name: " + name + " for " + function);
      }
      return Text.create(stringsLibrary.asString(name));
    } catch (UnsupportedMessageException e) {
      err.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError(builtins.function(), function, "function"), this);
    }
  }
}
