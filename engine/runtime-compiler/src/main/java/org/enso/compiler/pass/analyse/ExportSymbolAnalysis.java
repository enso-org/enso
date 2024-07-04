package org.enso.compiler.pass.analyse;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ImportTarget;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.compiler.pass.IRPass;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;

/**
 * This pass ensures that all the symbols that are exported exist. If not, an IR error is generated.
 * Iterates only exports that are not renamed and contain multiple symbols. For example:
 * {@code from project.Module export Symbol_1, Symbol_2 }.
 * All the other types of exports are assumed to already be resolved prior to this pass.
 */
public final class ExportSymbolAnalysis implements IRPass {
  public static final ExportSymbolAnalysis INSTANCE = new ExportSymbolAnalysis();
  private static scala.collection.immutable.List<IRPass> precursorPasses;
  private UUID uuid;

  private ExportSymbolAnalysis() {}

  @Override
  public UUID key() {
    return null;
  }

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    if (precursorPasses == null) {
      List<IRPass> passes = List.of(BindingAnalysis$.MODULE$, ImportSymbolAnalysis$.MODULE$);
      precursorPasses = CollectionConverters.asScala(passes).toList();
    }
    return precursorPasses;
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRPass> invalidatedPasses() {
    Object obj = scala.collection.immutable.Nil$.MODULE$;
    return (scala.collection.immutable.List<IRPass>) obj;
  }

  @Override
  public Module runModule(Module moduleIr, ModuleContext moduleContext) {
    List<Export> exportErrors = new ArrayList<>();
    var bindingsMap = (BindingsMap) moduleIr.passData().get(BindingAnalysis$.MODULE$).get();

    moduleIr
        .exports()
        .foreach(
            export -> {
                if (export instanceof Export.Module exportIr &&
                    exportIr.onlyNames().isDefined() &&
                    exportIr.rename().isEmpty()) {
                  var exportNameParts = exportIr.name().parts();
                  var lastName = exportNameParts.last().name();
                  assert exportNameParts.size() > 1;
                  var preLastName = exportNameParts.apply(exportNameParts.size() - 2).name();

                  // importTarget is the entity which we will check for the presence of symbols.
                  // It can either be ResolvedModule or ResolvedType.
                  ImportTarget importTarget = null;
                  // In correctly formed export, either `lastName` or `preLastName` must resolve
                  // to a module
                  var resolvedModule = findInDirectlyExportedModules(lastName, bindingsMap);
                  if (resolvedModule != null) {
                    importTarget = resolvedModule;
                  } else {
                    resolvedModule = findInDirectlyExportedModules(preLastName, bindingsMap);
                    if (resolvedModule == null) {
                      var err = ImportExport.apply(
                          exportIr,
                          new ImportExport.ModuleDoesNotExist(preLastName),
                          emptyPassData(),
                          emptyDiagnostics());
                      exportErrors.add(err);
                      return null;
                    }
                    var resolvedTypeOpt = resolvedModule.resolveExportedSymbol(lastName);
                    if (resolvedTypeOpt.isLeft()) {
                      var err = ImportExport.apply(
                          exportIr,
                          new ImportExport.TypeDoesNotExist(
                              lastName, preLastName),
                          emptyPassData(),
                          emptyDiagnostics());
                      exportErrors.add(err);
                      return null;
                    }
                    var resolvedType = resolvedTypeOpt.toOption().get();
                    if (resolvedType.size() > 1 || !(resolvedType.head() instanceof ResolvedType)) {
                      throw new CompilerError("Multiple resolved targets for a symbol, expected resolved type: " + resolvedType);
                    }
                    importTarget = (ImportTarget) resolvedType.head();
                  }
                  assert importTarget != null;

                  for (var exportedSymbol : CollectionConverters.asJava(exportIr.onlyNames().get())) {
                    var res = importTarget.resolveExportedSymbol(exportedSymbol.name());
                    if (res.isLeft()) {
                      var err = ImportExport.apply(
                          exportedSymbol,
                          new ImportExport.SymbolDoesNotExist(
                              exportedSymbol.name(), importTarget.qualifiedName().toString()),
                          emptyPassData(),
                          emptyDiagnostics());
                      exportErrors.add(err);
                    }
                  }
                }
              return null;
            }
        );

    if (exportErrors.isEmpty()) {
      return moduleIr;
    } else {
      return moduleIr.copy(
          moduleIr.imports(),
          CollectionConverters.asScala(exportErrors).toList(),
          moduleIr.bindings(),
          moduleIr.location(),
          moduleIr.passData(),
          moduleIr.diagnostics(),
          moduleIr.id());
    }
  }

  /**
   * Tries to find module with {@code name} name in modules that are _directly exported_ from
   * the current module.
   * @return null if not found.
   */
  private ResolvedModule findInDirectlyExportedModules(String name, BindingsMap bindingsMap) {
    assert !name.contains(".") : "Name must not be FQN";
    for (var exportedMod : CollectionConverters.asJava(bindingsMap.getDirectlyExportedModules())) {
      var exportedModName = exportedMod.module().qualifiedName().item();
      if (name.equals(exportedModName)) {
        return exportedMod.module();
      }
    }
    return null;
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }

  /**
   * Finds a resolved export that corresponds to the export IR.
   *
   * @param exportIr Export IR that is being resolved
   * @param bindingsMap Bindings map of the module that contains the export IR
   * @return null if no resolved export was found, otherwise the resolved export
   */
  private scala.collection.immutable.List<BindingsMap.ResolvedName> findResolvedExportForIr(
      Export exportIr, BindingsMap bindingsMap) {
    switch (exportIr) {
      case Export.Module exportedModIr -> {
        var exportedSymbolName = exportedModIr.name().parts().last().name();
        var resolvedNamesOpt = bindingsMap.exportedSymbols().get(exportedSymbolName);
        if (resolvedNamesOpt.isEmpty()) {
          return null;
        } else {
          return resolvedNamesOpt.get();
        }
      }
      default -> throw new IllegalStateException("Unexpected value: " + exportIr);
    }
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  private static MetadataStorage emptyPassData() {
    return new MetadataStorage();
  }

  @SuppressWarnings("unchecked")
  private static DiagnosticStorage emptyDiagnostics() {
    return DiagnosticStorage.apply((Seq<Diagnostic>) Seq$.MODULE$.empty());
  }

}
