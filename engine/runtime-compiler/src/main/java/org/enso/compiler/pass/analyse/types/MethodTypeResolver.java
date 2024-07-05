package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.pass.analyse.types.scope.ModuleResolver;
import org.enso.compiler.pass.analyse.types.scope.StaticModuleScope;
import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class MethodTypeResolver {
  private static final Logger logger = LoggerFactory.getLogger(MethodTypeResolver.class);
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
    if (definitionModule != null) {
      var definitionScope = StaticModuleScope.forIR(definitionModule);
      var definedWithAtom = definitionScope.getMethodForType(type, methodName);
      if (definedWithAtom != null) {
        return definedWithAtom;
      }
    } else {
      logger.error("Could not find declaration module of type: {}", type);
    }

    var definedHere = currentModuleScope.getMethodForType(type, methodName);
    if (definedHere != null) {
      return definedHere;
    }

    // TODO check for imports
    return null;
  }
}
