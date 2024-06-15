package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.pass.analyse.types.scope.ModuleResolver;
import org.enso.compiler.pass.analyse.types.scope.StaticModuleScope;
import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;

class MethodTypeResolver {
  private final ModuleResolver moduleResolver;
  private final StaticModuleScope currentModuleScope;

  MethodTypeResolver(ModuleResolver moduleResolver, StaticModuleScope currentModuleScope) {
    this.moduleResolver = moduleResolver;
    this.currentModuleScope = currentModuleScope;
  }

  // This should be aligned with
  // TODO extract common logic with ModuleScope::lookupMethodDefinition
  TypeRepresentation resolveMethod(TypeScopeReference type, String methodName) {
    var definitionModule = moduleResolver.findContainingModule(type);
    var definitionScope = StaticModuleScope.forIR(definitionModule);
    var definedWithAtom = definitionScope.getMethodForType(type, methodName);
    if (definedWithAtom != null) {
      return definedWithAtom;
    }

    var definedHere = currentModuleScope.getMethodForType(type, methodName);
    if (definedHere != null) {
      return definedHere;
    }

    // TODO check for imports
    return null;
  }
}
