package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.resource.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin Managed_Resource types */
public class Resource {

  /**
   * Creates and registers the relevant constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors in.
   */
  public Resource(Language language, ModuleScope scope) {
    AtomConstructor managedResource =
        new AtomConstructor("Managed_Resource", scope).initializeFields();
    scope.registerConstructor(managedResource);
    AtomConstructor resource = new AtomConstructor("Resource", scope).initializeFields();
    scope.registerConstructor(resource);
    scope.registerMethod(resource, "bracket", BracketMethodGen.makeFunction(language));
    scope.registerMethod(managedResource, "register", RegisterMethodGen.makeFunction(language));
    scope.registerMethod(managedResource, "with", WithMethodGen.makeFunction(language));
    scope.registerMethod(managedResource, "take", TakeMethodGen.makeFunction(language));
    scope.registerMethod(managedResource, "finalize", FinalizeMethodGen.makeFunction(language));
  }
}
