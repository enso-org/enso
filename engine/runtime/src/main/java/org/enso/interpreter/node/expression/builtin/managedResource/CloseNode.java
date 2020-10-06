package org.enso.interpreter.node.expression.builtin.managedResource;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.ManagedResource;

@BuiltinMethod(
    type = "Managed_Resource",
    name = "close",
    description = "Closes a managed resource, even if it is still reachable.")
public abstract class CloseNode extends Node {

  static CloseNode build() {
    return CloseNodeGen.create();
  }

  abstract Object execute(Object _this, ManagedResource resource);

  @Specialization
  Object doClose(Object _this, ManagedResource resource, @CachedContext(Language.class) Context context) {
    context.getResourceManager().close(resource);
    return context.getBuiltins().unit().newInstance();
  }
}
