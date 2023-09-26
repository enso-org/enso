package org.enso.compiler.pass.analyse;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRPass;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.QualifiedName;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Iterates through all the imports and exports of non-synthetic modules and ensures that:
 * <ul>
 *   <li>No private module is exported</li>
 *   <li>No private module from a different project is imported</li>
 * </ul>
 * Inserts errors into imports/exports IRs if the above conditions are violated.
 */
public final class PrivateModuleAnalysis implements IRPass {
  public static final PrivateModuleAnalysis MODULE$ = new PrivateModuleAnalysis();
  private UUID uuid;

  private PrivateModuleAnalysis() {}

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public UUID key() {
    return uuid;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    List<IRPass> passes = List.of(
        BindingAnalysis$.MODULE$,
        ImportSymbolAnalysis$.MODULE$
    );
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRPass> invalidatedPasses() {
    return ScalaConversions.nil();
  }

  @Override
  public Module runModule(Module moduleIr, ModuleContext moduleContext) {
    if (moduleContext.isSynthetic()) {
      return moduleIr;
    }

    var bindingsMap = (BindingsMap) moduleIr.passData().get(BindingAnalysis$.MODULE$).get();
    var currentPackage = moduleContext.getPackage();
    List<Import> importErrors = new ArrayList<>();
    List<Export> exportErrors = new ArrayList<>();

    // Check if imported modules are not private
    bindingsMap.resolvedImports().foreach(resolvedImp -> {
      var importedModule = resolvedImp.target().module().unsafeAsModule("should succeed");
      var importedModuleName = importedModule.getName().toString();
      var importedModulePackage = importedModule.getPackage();
      if (currentPackage != null
          && !currentPackage.equals(importedModulePackage)
          && importedModule.isPrivate()) {
        importErrors.add(ImportExport.apply(
            resolvedImp.importDef(),
            new ImportExport.ImportPrivateModule(importedModuleName),
            ImportExport.apply$default$3(),
            ImportExport.apply$default$4()
        ));
      }
      return null;
    });

    // If this module is private, we cannot export anything from it.
    if (moduleIr.isPrivate() && containsExport(moduleIr)) {
      exportErrors.add(ImportExport.apply(
          moduleIr.exports().apply(0),
          new ImportExport.ExportSymbolsFromPrivateModule(moduleContext.getName().toString()),
          ImportExport.apply$default$3(),
          ImportExport.apply$default$4()
      ));
    }

    // Check if we try to export some other private module.
    bindingsMap
        .getDirectlyExportedModules()
        .foreach(expModule -> {
          var expModuleRef = expModule.target().module().unsafeAsModule("should succeed");
          if (expModuleRef.isPrivate()) {
            var associatedExportIR = findExportIRByName(moduleIr, expModuleRef.getName());
            assert associatedExportIR.isDefined();
            exportErrors.add(
                ImportExport.apply(
                    associatedExportIR.get(),
                    new ImportExport.ExportPrivateModule(expModuleRef.getName().toString()),
                    ImportExport.apply$default$3(),
                    ImportExport.apply$default$4()
                )
            );
          }
          return null;
        });

    scala.collection.immutable.List<Import> convertedImports =
        importErrors.isEmpty() ? moduleIr.imports() : CollectionConverters.asScala(importErrors).toList();
    scala.collection.immutable.List<Export> convertedExports =
        exportErrors.isEmpty() ? moduleIr.exports() : CollectionConverters.asScala(exportErrors).toList();

    return moduleIr.copy(
        convertedImports,
        convertedExports,
        moduleIr.bindings(),
        moduleIr.location(),
        moduleIr.passData(),
        moduleIr.diagnostics(),
        moduleIr.id()
    );
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }

  /**
   * Returns true iff the given Module's IR contains an export that is not synthetic.
   */
  private static boolean containsExport(Module moduleIr) {
    return !moduleIr.exports().isEmpty() && moduleIr.exports().exists(exp -> {
      if (exp instanceof Export.Module moduleExport) {
        return !moduleExport.isSynthetic();
      } else {
        return false;
      }
    });
  }

  private static Option<Export> findExportIRByName(Module moduleIr, QualifiedName fqn) {
    return moduleIr.exports().find(exp -> {
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

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }
}
