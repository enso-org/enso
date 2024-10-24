package org.enso.compiler.context;

import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

/**
 * Encapsulates the logic for resolving a method call on a type/module.
 *
 * <p>The same logic is needed in two places:
 *
 * <ol>
 *   <li>in the runtime ({@link
 *       org.enso.interpreter.runtime.scope.ModuleScope#lookupMethodDefinition}),
 *   <li>in the type checker ({@link
 *       org.enso.compiler.pass.analyse.types.MethodTypeResolver#lookupMethodDefinition}).
 * </ol>
 *
 * <p>To ensure that all usages stay consistent, they should all rely on the logic implemented in
 * this class, customizing it to the specific needs of the context in which it is used.
 *
 * @param <FunctionType> The type of resolved method - the result type of the resolution process.
 * @param <TypeScopeReferenceType> TODO
 */
public abstract class MethodResolutionAlgorithm<
    FunctionType,
    TypeScopeReferenceType,
    ImportExportScopeType,
    ModuleScopeType extends
        MethodResolutionAlgorithm.CommonModuleScope<
                FunctionType, TypeScopeReferenceType, ImportExportScopeType>> {
  interface CommonModuleScope<FunctionType, TypeScopeReferenceType, ImportExportScopeType> {
    FunctionType getMethodForType(TypeScopeReferenceType type, String methodName);

    Collection<ImportExportScopeType> getImports();
  }

  public FunctionType lookupMethodDefinition(TypeScopeReferenceType type, String methodName) {
    var definitionScope = findDefinitionScope(type);
    if (definitionScope != null) {
      var definedWithAtom = definitionScope.getMethodForType(type, methodName);
      if (definedWithAtom != null) {
        return definedWithAtom;
      }
    }

    var definedHere = getCurrentModuleScope().getMethodForType(type, methodName);
    if (definedHere != null) {
      return definedHere;
    }

    return findInImports(type, methodName);
  }

  private FunctionType findInImports(TypeScopeReferenceType type, String methodName) {
    var found =
        getCurrentModuleScope().getImports().stream()
            .flatMap(
                (importExportScope) -> {
                  var exportedMethod =
                      findExportedMethodInImportScope(importExportScope, type, methodName);
                  if (exportedMethod != null) {
                    return Stream.of(new MethodFromImport<>(exportedMethod, importExportScope));
                  } else {
                    return Stream.empty();
                  }
                })
            .toList();

    if (found.size() == 1) {
      return found.get(0).resolvedType;
    } else if (found.size() > 1) {
      return onMultipleDefinitionsFromImports(found);
    } else {
      return null;
    }
  }

  protected abstract ModuleScopeType findDefinitionScope(TypeScopeReferenceType type);

  protected abstract ModuleScopeType getCurrentModuleScope();

  protected abstract FunctionType findExportedMethodInImportScope(
      ImportExportScopeType importExportScope, TypeScopeReferenceType type, String methodName);

  protected abstract FunctionType onMultipleDefinitionsFromImports(
      List<MethodFromImport<FunctionType, ImportExportScopeType>> imports);

  protected record MethodFromImport<FunctionType, ImportExportScopeType>(
      FunctionType resolvedType, ImportExportScopeType origin) {}
}
