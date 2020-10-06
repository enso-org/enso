package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.managedResource.FinalizeMethodGen;
import org.enso.interpreter.node.expression.builtin.managedResource.RegisterMethodGen;
import org.enso.interpreter.node.expression.builtin.managedResource.TakeMethodGen;
import org.enso.interpreter.node.expression.builtin.managedResource.WithMethodGen;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin Managed_Resource types */
public class ManagedResource {

  /**
   * Creates and registers the relevant constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors in.
   */
  public ManagedResource(Language language, ModuleScope scope) {
    AtomConstructor resource = new AtomConstructor("Managed_Resource", scope).initializeFields();
    scope.registerConstructor(resource);
    scope.registerMethod(resource, "register", RegisterMethodGen.makeFunction(language));
    scope.registerMethod(resource, "with", WithMethodGen.makeFunction(language));
    scope.registerMethod(resource, "take", TakeMethodGen.makeFunction(language));
    scope.registerMethod(resource, "finalize", FinalizeMethodGen.makeFunction(language));
  }
}
