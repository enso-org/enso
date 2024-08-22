package org.enso.compiler.phase.exports;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.compiler.pass.IRPass;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Ensures that all the symbols that are exported exist. If not, an IR error is generated. It is not
 * a {@link IRPass compiler pass} because it needs access to {@link
 * org.enso.compiler.PackageRepository}. Ignores renamed exports.
 */
public final class ExportSymbolAnalysis {
  private ExportSymbolAnalysis() {}

  /**
   * For all the exports in the given IR of the module, ensures that all the symbols that are
   * exported exists. If not, the corresponding IR is replaced with an {@link ImportExport error
   * IR}.
   *
   * @param moduleIr IR of the module to be analyzed for existance of exported symbols.
   * @return A copy of the given {@code moduleIr} optinally with some export IRs replaced with
   *     corresponding {@link ImportExport errors}.
   */
  public static Module analyseModule(Module moduleIr, PackageRepository packageRepository) {
    List<Export> exportErrors = new ArrayList<>();
    moduleIr
        .exports()
        .foreach(
            export -> {
              if (export instanceof Export.Module exportIr && exportIr.rename().isEmpty()) {
                var exportNameParts = CollectionConverters.asJava(exportIr.name().parts());
                assert exportNameParts.size() > 2
                    : "All the exports should already be desugared in the module discovery compiler"
                        + " passes";

                if (exportNameParts.size() == 3) {
                  // There are 3 parts of the export, which means that it should be a direct
                  // module export like `export namespace.project.Module`, or
                  // `from namespace.project.Module export Symbol`.
                  var modFQN =
                      exportNameParts.stream().map(Name::name).collect(Collectors.joining("."));
                  var mod = getLoadedModule(modFQN, packageRepository);
                  if (mod == null) {
                    var err = createModuleDoesNotExistError(exportIr, modFQN);
                    exportErrors.add(err);
                    return null;
                  }
                  if (exportIr.onlyNames().isDefined()) {
                    var symbols =
                        CollectionConverters.asJava(exportIr.onlyNames().get()).stream()
                            .map(Name.class::cast)
                            .toList();
                    var errs = analyseSymbolsFromModule(mod, symbols);
                    exportErrors.addAll(errs);
                  }
                  return null;
                }

                assert exportNameParts.size() > 3;

                String lastNameFQN;
                String lastNameItem;
                String preLastNameFQN;
                List<Name> symbols;
                if (exportIr.onlyNames().isDefined()) {
                  lastNameItem = exportNameParts.get(exportNameParts.size() - 1).name();
                  lastNameFQN =
                      exportNameParts.stream().map(Name::name).collect(Collectors.joining("."));
                  preLastNameFQN =
                      exportNameParts.stream()
                          .map(Name::name)
                          .limit(exportNameParts.size() - 1)
                          .collect(Collectors.joining("."));
                  symbols =
                      CollectionConverters.asJava(exportIr.onlyNames().get()).stream()
                          .map(Name.class::cast)
                          .toList();
                } else {
                  lastNameItem = exportNameParts.get(exportNameParts.size() - 2).name();
                  lastNameFQN =
                      exportNameParts.stream()
                          .map(Name::name)
                          .limit(exportNameParts.size() - 1)
                          .collect(Collectors.joining("."));
                  preLastNameFQN =
                      exportNameParts.stream()
                          .map(Name::name)
                          .limit(exportNameParts.size() - 2)
                          .collect(Collectors.joining("."));
                  symbols = List.of(exportNameParts.get(exportNameParts.size() - 1));
                }

                var mod = getLoadedModule(lastNameFQN, packageRepository);
                if (mod != null) {
                  var errs = analyseSymbolsFromModule(mod, symbols);
                  if (!errs.isEmpty()) {
                    // It is also possible that symbol refers to a submodule of this module
                    var subModFQN =
                        exportNameParts.stream().map(Name::name).collect(Collectors.joining("."));
                    var subMod = getLoadedModule(subModFQN, packageRepository);
                    if (subMod != null) {
                      // Everything is OK - we export the subMod directly
                      return null;
                    }
                  }
                  exportErrors.addAll(errs);
                  return null;
                }
                mod = getLoadedModule(preLastNameFQN, packageRepository);
                if (mod == null) {
                  var err = createModuleDoesNotExistError(exportIr, preLastNameFQN);
                  exportErrors.add(err);
                  return null;
                }
                // lastNameFQN must be type in module `mod`
                var modFQN = preLastNameFQN;
                var typeName = lastNameItem;
                var resolvedType = getResolvedTypeFromModule(mod, typeName);
                if (resolvedType == null) {
                  var err = createTypeDoesNotExistError(exportIr, modFQN, typeName);
                  exportErrors.add(err);
                  return null;
                }
                var errs = analyseSymbolsFromType(resolvedType, symbols);
                exportErrors.addAll(errs);
              }
              return null;
            });

    if (exportErrors.isEmpty()) {
      return moduleIr;
    } else {
      return moduleIr.copy(
          moduleIr.imports(),
          CollectionConverters.asScala(exportErrors).toList(),
          moduleIr.bindings(),
          moduleIr.isPrivate(),
          moduleIr.location(),
          moduleIr.passData(),
          moduleIr.diagnostics(),
          moduleIr.id());
    }
  }

  private static CompilerContext.Module getLoadedModule(String modFQN, PackageRepository pkgRepo) {
    assert modFQN.contains(".") : "modFQN is a FQN";
    var modOpt = pkgRepo.getLoadedModule(modFQN);
    if (modOpt.isDefined()) {
      return modOpt.get();
    } else {
      return null;
    }
  }

  private static ResolvedType getResolvedTypeFromModule(
      CompilerContext.Module module, String typeName) {
    assert !typeName.contains(".") : "Not a FQN";
    var typeOpt = module.getBindingsMap().exportedSymbols().get(typeName);
    if (typeOpt.isEmpty()) {
      return null;
    }
    var resolvedNames = typeOpt.get();
    if (resolvedNames.size() > 1) {
      // We expect a single ResolvedType target
      return null;
    }
    if (resolvedNames.head() instanceof ResolvedType resolvedType) {
      return resolvedType;
    }
    return null;
  }

  /**
   * @return Optionally empty list of errors. Not null.
   */
  private static List<ImportExport> analyseSymbolsFromModule(
      CompilerContext.Module module, List<Name> symbols) {
    var bindingsMap = module.getBindingsMap();
    if (bindingsMap == null) {
      // This can happen if the module was not yet compiled. Which may happen.
      // In that case, just skip the check.
      return List.of();
    }
    var errors = new ArrayList<ImportExport>();
    for (var symbol : symbols) {
      var resolvedNamesOpt = bindingsMap.exportedSymbols().get(symbol.name());
      if (resolvedNamesOpt.isEmpty()) {
        errors.add(
            createSymbolDoesNotExistError(symbol, symbol.name(), module.getName().toString()));
      }
    }
    return errors;
  }

  /**
   * @return Optionally empty list of errors. Not null.
   */
  private static List<ImportExport> analyseSymbolsFromType(ResolvedType type, List<Name> symbols) {
    var errors = new ArrayList<ImportExport>();
    var constructors = CollectionConverters.asJava(type.tp().members());
    for (var symbol : symbols) {
      var symbolIsConstructor =
          constructors.stream().anyMatch(cons -> cons.name().equals(symbol.name()));
      if (!symbolIsConstructor) {
        errors.add(createNoSuchConstructorError(symbol, type.tp().name(), symbol.name()));
      }
    }
    return errors;
  }

  private static ImportExport createModuleDoesNotExistError(Export.Module exportIr, String modFQN) {
    assert modFQN.contains(".");
    return new ImportExport(exportIr, new ImportExport.ModuleDoesNotExist(modFQN), emptyPassData());
  }

  private static ImportExport createSymbolDoesNotExistError(
      Name symbolIr, String symbolName, String modFQN) {
    assert modFQN.contains(".");
    assert !symbolName.contains(".");
    return new ImportExport(
        symbolIr,
        new ImportExport.SymbolDoesNotExist(symbolName, modFQN),
        emptyPassData(),
        emptyDiagnostics());
  }

  private static ImportExport createNoSuchConstructorError(
      Name symbolIr, String typeName, String consName) {
    assert !consName.contains(".");
    return new ImportExport(
        symbolIr,
        new ImportExport.NoSuchConstructor(typeName, consName),
        emptyPassData(),
        emptyDiagnostics());
  }

  private static ImportExport createTypeDoesNotExistError(
      Export.Module exportIr, String modFQN, String typeName) {
    assert modFQN.contains(".");
    assert !typeName.contains(".");
    return new ImportExport(
        exportIr,
        new ImportExport.TypeDoesNotExist(typeName, modFQN),
        emptyPassData(),
        emptyDiagnostics());
  }

  private static MetadataStorage emptyPassData() {
    return new MetadataStorage();
  }

  @SuppressWarnings("unchecked")
  private static DiagnosticStorage emptyDiagnostics() {
    return new DiagnosticStorage(DiagnosticStorage.$lessinit$greater$default$1());
  }
}
