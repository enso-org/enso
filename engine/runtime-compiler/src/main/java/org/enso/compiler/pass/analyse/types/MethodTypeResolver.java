package org.enso.compiler.pass.analyse.types;

import java.util.stream.Stream;
import org.enso.compiler.pass.analyse.types.scope.BuiltinsFallbackScope;
import org.enso.compiler.pass.analyse.types.scope.ModuleResolver;
import org.enso.compiler.pass.analyse.types.scope.StaticImportExportScope;
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
  private final BuiltinsFallbackScope builtinsFallbackScope;

  MethodTypeResolver(
      ModuleResolver moduleResolver,
      StaticModuleScope currentModuleScope,
      BuiltinTypes builtinTypes) {
    this.moduleResolver = moduleResolver;
    this.currentModuleScope = currentModuleScope;
    this.builtinsFallbackScope = new BuiltinsFallbackScope(builtinTypes);
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

  /**
   * Looks up a method definition as seen from the current module.
   *
   * <p>The logic should be aligned with {@link
   * org.enso.interpreter.runtime.scope.ModuleScope#lookupMethodDefinition}.
   */
  private TypeRepresentation lookupMethodDefinition(TypeScopeReference type, String methodName) {
    var definitionScope = findDefinitionScope(type);
    if (definitionScope != null) {
      var definedWithAtom = definitionScope.getMethodForType(type, methodName);
      if (definedWithAtom != null) {
        return definedWithAtom;
      }
    }

    var definedHere = currentModuleScope.getMethodForType(type, methodName);
    if (definedHere != null) {
      return definedHere;
    }

    var foundInImports =
        currentModuleScope.getImports().stream()
            .flatMap(
                staticImportExportScope -> {
                  var materialized = staticImportExportScope.materialize(moduleResolver);
                  var found = materialized.getExportedMethod(type, methodName);
                  return Stream.ofNullable(found);
                })
            .toList();

    if (foundInImports.size() == 1) {
      return foundInImports.get(0);
    } else if (foundInImports.size() > 1) {
      // TODO in some cases it seems like this may be normal?
      var foundImports =
          currentModuleScope.getImports().stream()
              .flatMap(
                  staticImportExportScope -> {
                    var materialized = staticImportExportScope.materialize(moduleResolver);
                    var found = materialized.getExportedMethod(type, methodName);
                    if (found != null) {
                      return Stream.of(staticImportExportScope.getReferredModuleName());
                    } else {
                      return Stream.of();
                    }
                  })
              .toList();
      logger.debug("Method {} is coming from multiple imports: {}", methodName, foundImports);
      var foundTypes = foundInImports.stream().distinct();
      if (foundTypes.count() > 1) {
        logger.error(
            "Method {} is coming from multiple imports with different types: {}",
            methodName,
            foundTypes);
        return null;
      } else {
        // If all types are the same, just return the first one
        return foundInImports.get(0);
      }
    }

    return null;
  }

  private StaticModuleScope findDefinitionScope(TypeScopeReference type) {
    var definitionModule = moduleResolver.findContainingModule(type);
    if (definitionModule != null) {
      return StaticModuleScope.forIR(definitionModule);
    } else {
      if (type.equals(TypeScopeReference.ANY)) {
        // We have special handling for ANY: it points to Standard.Base.Any.Any, but that may not
        // always be imported.
        // The runtime falls back to Standard.Builtins.Main, but that modules does not contain any
        // type information, so it is not useful for us.
        // Instead we fall back to the hardcoded definitions of the 5 builtins of Any.
        return builtinsFallbackScope.fallbackAnyScope();
      } else {
        logger.error("Could not find declaration module of type: {}", type);
        return null;
      }
    }
  }
}
