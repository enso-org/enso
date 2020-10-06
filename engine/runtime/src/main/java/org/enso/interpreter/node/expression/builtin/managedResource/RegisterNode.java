package org.enso.interpreter.node.expression.builtin.managedResource;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.ManagedResource;

@BuiltinMethod(
    type = "Managed_Resource",
    name = "register",
    description =
        "Makes an object into a managed resource, automatically finalized when the returned object is garbage collected.")
public abstract class RegisterNode extends Node {
  static RegisterNode build() {
    return RegisterNodeGen.create();
  }

  abstract ManagedResource execute(Object _this, Object resource, Function finalizer);

  @Specialization
  @CompilerDirectives.TruffleBoundary
  ManagedResource doRegister(
      Object _this,
      Object resource,
      Function function,
      @CachedContext(Language.class) Context context) {
    return context.getResourceManager().register(resource, function);
  }
}
