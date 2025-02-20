package org.enso.compiler.pass.analyse;

import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Iterates through all the imports and exports of non-synthetic modules and ensures that:
 *
 * <ul>
 *   <li>No private module is exported
 *   <li>No private module from a different project is imported
 *   <li>Hierarchy of modules and submodules does not mix private and public modules
 * </ul>
 *
 * Inserts errors into imports/exports IRs if the above conditions are violated.
 */
public final class PrivateModuleAnalysis implements MiniPassFactory {
  public static final PrivateModuleAnalysis INSTANCE = new PrivateModuleAnalysis();

  private PrivateModuleAnalysis() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(BindingAnalysis$.MODULE$, ImportSymbolAnalysis.INSTANCE);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRProcessingPass> invalidatedPasses() {
    Object obj = scala.collection.immutable.Nil$.MODULE$;
    return (scala.collection.immutable.List<IRProcessingPass>) obj;
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return new Mini(
        moduleContext.bindingsAnalysis(),
        moduleContext.getName().toString(),
        moduleContext.getPackage(),
        moduleContext.isSynthetic());
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }

  private static final class Mini extends MiniIRPass {
    private final BindingsMap bindingsMap;
    private final String moduleName;
    private final Package<?> currentPackage;
    private final boolean isSynthetic;

    private Mini(
        BindingsMap bindingsMap,
        String moduleName,
        Package<?> currentPackage,
        boolean isSynthetic) {
      assert bindingsMap != null;
      assert moduleName != null;
      this.moduleName = moduleName;
      this.currentPackage = currentPackage;
      this.isSynthetic = isSynthetic;
      this.bindingsMap = bindingsMap;
    }

    @Override
    public MiniIRPass prepare(IR parent, Expression child) {
      return null;
    }

    @Override
    public Expression transformExpression(Expression expr) {
      throw new IllegalStateException("Should not be called - prepare returns null");
    }

    @Override
    public Module transformModule(Module moduleIr) {
      List<Import> importErrors = new ArrayList<>();
      List<Export> exportErrors = new ArrayList<>();
      var isCurrentModulePrivate = moduleIr.isPrivate();

      // Ensure that imported modules from a different project are not private.
      bindingsMap
          .resolvedImports()
          .foreach(
              resolvedImp -> {
                var importedTargets = resolvedImp.targets();
                importedTargets.foreach(
                    importedTarget -> {
                      var importedModule = importedTarget.module().unsafeAsModule("should succeed");
                      var importedModuleName = importedModule.getName().toString();
                      var importedModulePackage = importedModule.getPackage();
                      if (currentPackage != null
                          && !currentPackage.equals(importedModulePackage)
                          && importedModule.isPrivate()) {
                        importErrors.add(
                            ImportExport.apply(
                                resolvedImp.importDef(),
                                new ImportExport.ImportPrivateModule(importedModuleName),
                                new MetadataStorage()));
                      }
                      return null;
                    });
                return null;
              });

      // Ensure that no symbols are exported from a private module.
      if (isCurrentModulePrivate && containsExport(moduleIr)) {
        exportErrors.add(
            ImportExport.apply(
                moduleIr.exports().apply(0),
                new ImportExport.ExportSymbolsFromPrivateModule(moduleName),
                new MetadataStorage()));
      }

      // Ensure that private modules are not exported
      bindingsMap
          .getDirectlyExportedModules()
          .foreach(
              expModule -> {
                var expModuleRef = expModule.module().module().unsafeAsModule("should succeed");
                if (expModuleRef.isPrivate() && !isSynthetic) {
                  var associatedExportIR = findExportIRByName(moduleIr, expModuleRef.getName());
                  assert associatedExportIR.isDefined();
                  exportErrors.add(
                      ImportExport.apply(
                          associatedExportIR.get(),
                          new ImportExport.ExportPrivateModule(expModuleRef.getName().toString()),
                          new MetadataStorage()));
                }
                return null;
              });

      scala.collection.immutable.List<Import> convertedImports =
          importErrors.isEmpty()
              ? moduleIr.imports()
              : CollectionConverters.asScala(importErrors).toList();
      scala.collection.immutable.List<Export> convertedExports =
          exportErrors.isEmpty()
              ? moduleIr.exports()
              : CollectionConverters.asScala(exportErrors).toList();

      return moduleIr.copyWithImportsAndExports(convertedImports, convertedExports);
    }
  }

  /** Returns true iff the given Module's IR contains an export that is not synthetic. */
  private static boolean containsExport(Module moduleIr) {
    return !moduleIr.exports().isEmpty()
        && moduleIr
            .exports()
            .exists(
                exp -> {
                  if (exp instanceof Export.Module moduleExport) {
                    return !moduleExport.isSynthetic();
                  } else {
                    return false;
                  }
                });
  }

  private static Option<Export> findExportIRByName(Module moduleIr, QualifiedName fqn) {
    return moduleIr
        .exports()
        .find(
            exp -> {
              if (exp instanceof Export.Module expMod) {
                if (expMod.name().parts().last().name().equals(fqn.item())) {
                  return true;
                }
              } else {
                throw new IllegalStateException("unknown exp: " + exp);
              }
              return null;
            });
  }
}
