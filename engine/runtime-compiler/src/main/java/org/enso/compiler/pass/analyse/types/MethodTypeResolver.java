package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.pass.analyse.types.scope.ModuleResolver;
import org.enso.compiler.pass.analyse.types.scope.StaticMethodResolution;
import org.enso.compiler.pass.analyse.types.scope.StaticModuleScope;
import org.enso.compiler.pass.analyse.types.scope.TypeHierarchy;
import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;

/** A helper that deals with resolving types of method calls. */
class MethodTypeResolver {
  private final StaticModuleScope currentModuleScope;
  private final StaticMethodResolution methodResolutionAlgorithm;

  MethodTypeResolver(ModuleResolver moduleResolver, StaticModuleScope currentModuleScope) {
    this.currentModuleScope = currentModuleScope;
    this.methodResolutionAlgorithm = new StaticMethodResolution(moduleResolver);
  }

  TypeRepresentation resolveMethod(TypeScopeReference type, String methodName) {
    var definition =
        methodResolutionAlgorithm.lookupMethodDefinition(currentModuleScope, type, methodName);
    if (definition != null) {
      return definition;
    }

    // If not found in current scope, try parents
    var parent = TypeHierarchy.getParent(type);
    if (parent == null) {
      return null;
    }

    return resolveMethod(parent, methodName);
  }
}
