package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.pass.analyse.types.scope.ModuleResolver;
import org.enso.compiler.pass.analyse.types.scope.StaticModuleScope;
import org.enso.compiler.pass.analyse.types.scope.TypeHierarchy;
import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class MethodTypeResolver {
  private static final Logger logger = LoggerFactory.getLogger(MethodTypeResolver.class);
  private final ModuleResolver moduleResolver;
  private final TypeHierarchy typeHierarchy = new TypeHierarchy();
  private final StaticModuleScope currentModuleScope;

  MethodTypeResolver(ModuleResolver moduleResolver, StaticModuleScope currentModuleScope) {
    this.moduleResolver = moduleResolver;
    this.currentModuleScope = currentModuleScope;
  }

  TypeRepresentation resolveMethod(TypeScopeReference type, String methodName) {
    var definition = lookupMethodDefinition(type, methodName);
    if (definition != null) {
      return definition;
    }

    // If not found in current scope, try parents
    var parent = typeHierarchy.getParent(type);
    if (parent == null) {
      return null;
    }

    return resolveMethod(parent, methodName);
  }

  // This should be aligned with
  // TODO extract common logic with ModuleScope::lookupMethodDefinition
  private TypeRepresentation lookupMethodDefinition(TypeScopeReference type, String methodName) {
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
