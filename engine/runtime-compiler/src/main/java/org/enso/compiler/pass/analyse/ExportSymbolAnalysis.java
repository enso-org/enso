package org.enso.compiler.pass.analyse;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRPass;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * This pass ensures that all the symbols that are exported exist. If not, an IR error is generated.
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
            export ->
                switch (export) {
                  case Export.Module exportMod -> {
                    var exportNameParts = exportMod.name().parts();
                    var symbolName = exportMod.name().parts().last();
                    assert exportNameParts.size() > 1;
                    var moduleOrTypeName = exportNameParts.apply(exportNameParts.size() - 2);
                    var foundResolvedNames = findResolvedExportForIr(export, bindingsMap);
                    if (foundResolvedNames == null) {
                      exportErrors.add(
                          ImportExport.apply(
                              symbolName,
                              new ImportExport.SymbolDoesNotExist(
                                  symbolName.name(), moduleOrTypeName.name()),
                              ImportExport.apply$default$3(),
                              ImportExport.apply$default$4()));
                    } else {
                      if (exportMod.onlyNames().isDefined()) {
                        assert exportMod.onlyNames().isDefined();
                        var exportedSymbols = exportMod.onlyNames().get();
                        exportedSymbols.foreach(
                            exportedSymbol -> {
                              foundResolvedNames.foreach(resolvedName -> {
                                var bm = resolvedName.module().unsafeAsModule("Should be defined").getBindingsMap();
                                if (!bm.exportedSymbols().contains(exportedSymbol.name())) {
                                  exportErrors.add(
                                      ImportExport.apply(
                                          exportedSymbol,
                                          new ImportExport.SymbolDoesNotExist(
                                              exportedSymbol.name(), moduleOrTypeName.name()),
                                          ImportExport.apply$default$3(),
                                          ImportExport.apply$default$4()));
                                }
                                return null;
                              });
                              return null;
                            });
                      }
                    }
                    yield null;
                  }
                  default -> export;
                });

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
}
