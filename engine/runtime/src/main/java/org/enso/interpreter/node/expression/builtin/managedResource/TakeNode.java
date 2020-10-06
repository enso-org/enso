package org.enso.interpreter.node.expression.builtin.managedResource;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Resource;

@BuiltinMethod(
    type = "Managed_Resource",
    name = "unsafe_take",
    description =
        "Takes the value held by the managed resource and removes the finalization callbacks,"
            + " effectively making the underlying resource unmanaged again.")
public abstract class TakeNode extends Node {

  static TakeNode build() {
    return TakeNodeGen.create();
  }

  abstract Object execute(Object _this, Resource resource);

  @Specialization
  Object doTake(Object _this, Resource resource, @CachedContext(Language.class) Context context) {
    context.getResourceManager().take(resource);
    return resource.getResource();
  }
}
