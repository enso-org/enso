package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;

class MethodTypeResolver {
  private final ModuleResolver moduleResolver;

  MethodTypeResolver(ModuleResolver moduleResolver) {
    this.moduleResolver = moduleResolver;
  }

  // This should be aligned with
  // TODO extract common logic
  TypeRepresentation resolveMethod(TypeScopeReference type, String methodName) {
    // TODO
    return null;
  }
}
