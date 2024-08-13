package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.pass.analyse.types.scope.ModuleResolver;
import org.enso.compiler.pass.analyse.types.scope.StaticModuleScope;
import org.enso.compiler.pass.analyse.types.scope.TypeHierarchy;
import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.stream.Stream;

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
  // I wanted to keep this decoupled from StaticModuleScope to keep it as a pure-data class
  private TypeRepresentation lookupMethodDefinition(TypeScopeReference type, String methodName) {
    var definitionModule = moduleResolver.findContainingModule(type);
    if (definitionModule != null) {
      var definitionScope = StaticModuleScope.forIR(definitionModule);
      var definedWithAtom = definitionScope.getMethodForType(type, methodName);
      if (definedWithAtom != null) {
        return definedWithAtom;
      }
    } else {
      // TODO need a way to find builtin Any
      logger.error("Could not find declaration module of type: {}", type);
    }

    var definedHere = currentModuleScope.getMethodForType(type, methodName);
    if (definedHere != null) {
      return definedHere;
    }

    var foundInImports = currentModuleScope.getImports().stream().flatMap(staticImportExportScope -> {
      var materialized = staticImportExportScope.materialize(moduleResolver);
      return Stream.of(materialized.getMethodForType(type, methodName));
    }).toList();

    if (foundInImports.size() == 1) {
      return foundInImports.get(0);
    } else if (foundInImports.size() > 1) {
      // TODO we'd like to report this as a diagnostic
      logger.error("Method {} is defined in multiple imports: {}", methodName, foundInImports);
    }

    return null;
  }
}
