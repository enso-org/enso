package org.enso.interpreter.node.expression.builtin.resource;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Resource;

@BuiltinMethod(type = "IO", name = "println", description = "Prints its argument to standard out.")
public abstract class RegisterNode extends Node {
  static RegisterNode build() {
    return RegisterNodeGen.create();
  }

  abstract Resource execute(Object _this, Object resource, Function function);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Resource doRegister(
      Object _this,
      Object resource,
      Function function,
      @CachedContext(Language.class) Context context) {
    return context.getResourceManager().register(resource, function);
  }
}
