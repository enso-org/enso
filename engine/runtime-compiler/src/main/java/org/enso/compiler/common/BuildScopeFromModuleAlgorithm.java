package org.enso.compiler.common;

import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.resolve.MethodDefinitions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Gathers the common logic for building the ModuleScope.
 *
 * <p>This is done in two places:
 *
 * <ol>
 *   <li>in the compiler, gathering just the types to build StaticModuleScope,
 *   <li>in the runtime, building Truffle nodes for the interpreter.
 * </ol>
 *
 * <p>The interpreter does much more than the type-checker, so currently this only gathers the
 * general shape of the process to try to ensure that they stay in sync. In future iterations, we
 * may try to move more of the logic to this common place.
 */
public abstract class BuildScopeFromModuleAlgorithm<TypeScopeReferenceType, ImportExportScopeType> {
  private final Logger logger = LoggerFactory.getLogger(BuildScopeFromModuleAlgorithm.class);

  protected abstract void registerExport(ImportExportScopeType exportScope);

  protected abstract void registerImport(ImportExportScopeType importScope);

  protected abstract TypeScopeReferenceType getTypeAssociatedWithCurrentScope();

  /** Runs the main processing on a module, that will build the module scope for it. */
  public final void processModule(Module moduleIr, BindingsMap bindingsMap) {
    processModuleExports(bindingsMap);
    processModuleImports(bindingsMap);
    processPolyglotImports(moduleIr);

    processBindings(moduleIr);
  }

  private void processModuleExports(BindingsMap bindingsMap) {
    for (var exportedMod :
        CollectionConverters.asJavaCollection(bindingsMap.getDirectlyExportedModules())) {
      ImportExportScopeType exportScope = buildExportScope(exportedMod);
      registerExport(exportScope);
    }
  }

  private void processModuleImports(BindingsMap bindingsMap) {
    for (var imp : CollectionConverters.asJavaCollection(bindingsMap.resolvedImports())) {
      for (var target : CollectionConverters.asJavaCollection(imp.targets())) {
        if (target instanceof BindingsMap.ResolvedModule resolvedModule) {
          var importScope = buildImportScope(imp, resolvedModule);
          registerImport(importScope);
        }
      }
    }
  }

  private void processPolyglotImports(Module moduleIr) {
    for (var imp : CollectionConverters.asJavaCollection(moduleIr.imports())) {
      if (imp instanceof Polyglot polyglotImport) {
        if (polyglotImport.entity() instanceof Polyglot.Java javaEntity) {
          processPolyglotJavaImport(polyglotImport.getVisibleName(), javaEntity.getJavaName());
        } else {
          throw new IllegalStateException(
              "Unsupported polyglot import entity: " + polyglotImport.entity());
        }
      }
    }
  }

  private void processBindings(Module module) {
    for (var binding : CollectionConverters.asJavaCollection(module.bindings())) {
      switch (binding) {
        case Definition.Type typ -> processTypeDefinition(typ);
        case Method.Explicit method -> processMethodDefinition(method);
        case Method.Conversion conversion -> processConversion(conversion);
        default -> logger.warn(
            "Unexpected binding type: {}", binding.getClass().getCanonicalName());
      }
    }
  }

  /** Allows the implementation to specify how to register polyglot Java imports. */
  protected abstract void processPolyglotJavaImport(String visibleName, String javaClassName);

  /**
   * Allows the implementation to specify how to register conversions.
   *
   * <p>In the future we may want to extract some common logic from this, but for now we allow the
   * implementation to specify this.
   */
  protected abstract void processConversion(Method.Conversion conversion);

  /** Allows the implementation to specify how to register method definitions. */
  protected abstract void processMethodDefinition(Method.Explicit method);

  /**
   * Allows the implementation to specify how to register type definitions, along with their
   * constructors and getters.
   *
   * <p>The type registration (registering constructors, getters) is really complex, ideally we'd
   * also like to extract some common logic from it. But the differences are very large, so setting
   * that aside for later.
   */
  protected abstract void processTypeDefinition(Definition.Type typ);

  /**
   * Common method that allows to extract the type on which the method is defined.
   *
   * <ul>
   *   <li>For a member method, this will be its parent type.
   *   <li>For a static method, this will be the eigentype of the type on which it is defined.
   *   <li>For a module method, this will be the type associated with the module.
   * </ul>
   */
  protected final TypeScopeReferenceType getTypeDefiningMethod(Method.Explicit method) {
    var typePointerOpt = method.methodReference().typePointer();
    if (typePointerOpt.isEmpty()) {
      return getTypeAssociatedWithCurrentScope();
    } else {
      var metadata =
          MetadataInteropHelpers.getMetadataOrNull(
              typePointerOpt.get(), MethodDefinitions.INSTANCE, BindingsMap.Resolution.class);
      if (metadata == null) {
        logger.debug(
            "Failed to resolve type pointer for method: {}", method.methodReference().showCode());
        return null;
      }

      return switch (metadata.target()) {
        case BindingsMap.ResolvedType resolvedType -> associatedTypeFromResolvedType(
            resolvedType, method.isStatic());
        case BindingsMap.ResolvedModule resolvedModule -> associatedTypeFromResolvedModule(
            resolvedModule);
        default -> throw new IllegalStateException(
            "Unexpected target type: " + metadata.target().getClass().getCanonicalName());
      };
    }
  }

  /**
   * Implementation specific piece of {@link #getTypeDefiningMethod(Method.Explicit)} that specifies
   * how to build the associated type from a resolved module.
   */
  protected abstract TypeScopeReferenceType associatedTypeFromResolvedModule(
      BindingsMap.ResolvedModule module);

  /**
   * Implementation specific piece of {@link #getTypeDefiningMethod(Method.Explicit)} that specifies
   * how to build the associated type from a resolved type, depending on if the method is static or
   * not.
   */
  protected abstract TypeScopeReferenceType associatedTypeFromResolvedType(
      BindingsMap.ResolvedType type, boolean isStatic);

  /**
   * Allows the implementation to specify how to build the export scope from an exported module
   * instance.
   *
   * <p>Such scope is then registered with the scope builder using {@code addExport}.
   */
  protected abstract ImportExportScopeType buildExportScope(
      BindingsMap.ExportedModule exportedModule);

  /**
   * Allows the implementation to specify how to build the import scope from a resolved import and
   * module.
   *
   * <p>Such scope is then registered with the scope builder using {@code addImport}.
   */
  protected abstract ImportExportScopeType buildImportScope(
      BindingsMap.ResolvedImport resolvedImport, BindingsMap.ResolvedModule resolvedModule);
}
