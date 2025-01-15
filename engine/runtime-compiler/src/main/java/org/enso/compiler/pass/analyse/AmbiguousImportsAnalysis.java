package org.enso.compiler.pass.analyse;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.enso.scala.wrapper.ScalaConversions;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ImportTarget;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * A pass that checks for ambiguous and duplicated symbols from imports. A duplicated import is an
 * import of a symbol that has already been imported and refers to the same object (entity). On the
 * other hand, an ambiguous import is an import of a symbol that has already been imported but
 * refers to a different object. For every duplicated import, a warning is attached to the IR, and
 * for every ambiguous import, the IR is replaced with an error. To identify an object, this pass
 * uses physical path of the object instead of the object itself.
 *
 * <p>One import IR can be replaced with multiple error IRs. This is the case for {@code from ...
 * import ...} import statements.
 *
 * <p>The original import is saved in the error and warning so that the user can see from which
 * location the symbol was originally imported.
 *
 * <p>Also iterates polyglot imports.
 *
 * <p>All synthetic imports and exports, as well as synthetic modules are ignored by this pass.
 *
 * <p>This pass does not alter any metadata.
 */
public final class AmbiguousImportsAnalysis implements MiniPassFactory {
  public static final AmbiguousImportsAnalysis INSTANCE = new AmbiguousImportsAnalysis();

  private AmbiguousImportsAnalysis() {}

  @Override
  public Seq<? extends IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(BindingAnalysis$.MODULE$, ImportSymbolAnalysis.INSTANCE);
    return ScalaConversions.seq(passes);
  }

  @Override
  public Seq<? extends IRProcessingPass> invalidatedPasses() {
    return ScalaConversions.seq(List.of());
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    if (moduleContext.isSynthetic()) {
      return null;
    }
    return new Mini(moduleContext.bindingsAnalysis());
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }


  private static final class Mini extends MiniIRPass {
    private final EncounteredSymbols encounteredSymbols = new EncounteredSymbols();
    private final BindingsMap bindingsMap;

    private Mini(BindingsMap bindingsMap) {
      assert bindingsMap != null;
      this.bindingsMap = bindingsMap;
    }

    @Override
    public MiniIRPass prepare(IR parent, Expression child) {
      // return null - do not traverse any children of the root - we just
      // need to transform the module IR.
      return null;
    }

    @Override
    public Expression transformExpression(Expression expr) {
      throw new IllegalStateException("Should not be called - prepare returns null");
    }

    @Override
    public Module transformModule(Module moduleIr) {
      var newImports = new ArrayList<Import>();
      moduleIr.imports().foreach(imp -> {
        var errs = analyseAmbiguousSymbols(imp);
        if (!errs.isEmpty()) {
          newImports.addAll(errs);
        } else {
          newImports.add(imp);
        }
        return null;
      });
      return moduleIr.copy(
          CollectionConverters.asScala(newImports).toList(),
          moduleIr.exports(),
          moduleIr.bindings(),
          moduleIr.isPrivate(),
          moduleIr.location(),
          moduleIr.passData(),
          moduleIr.diagnostics(),
          moduleIr.id());
    }

    /**
     * @param imp Current import to analyse. May attach warnings.
     * @return List of collected errors. Potentially empty. Not null
     */
    private List<ImportExport> analyseAmbiguousSymbols(Import imp) {
      var errorsForImport = new ArrayList<ImportExport>();

      switch (imp) {
        // Import multiple symbols
        case Import.Module impMod when impMod.onlyNames().isDefined() && !impMod.isSynthetic() -> {
          for (var symbol : CollectionConverters.asJava(impMod.onlyNames().get())) {
            var symbolName = symbol.name();
            for (var importTarget : getImportTargets(impMod)) {
              var resolution = importTarget.resolveExportedSymbol(symbolName);
              if (resolution.isLeft()) {
                throw new CompilerError(
                    "Unreachable: (should have been resolved in previous passes): " + resolution);
              }
              var resolvedNames = resolution.toOption().get();
              for (var resolvedName : CollectionConverters.asJava(resolvedNames)) {
                var symbolPath = resolvedName.qualifiedName().toString();
                tryAddEncounteredSymbol(impMod, symbolName, symbolPath, resolvedName, errorsForImport);
              }
            }
          }
        }

        // Import all symbols
        case Import.Module impMod when impMod.isAll() && !impMod.isSynthetic() -> {
          var importTargets = getImportTargets(impMod);
          // Names of the symbols that are exported by a module or a type referred to via importTarget
          var exportedSymbolNames = importTargets.stream()
              .flatMap(target -> {
                var expSymbols = target.exportedSymbols().keySet().toList();
                return CollectionConverters.asJava(expSymbols).stream();
              });
          List<String> symbolsToIterate;
          if (impMod.hiddenNames().isDefined()) {
            var hiddenNames = impMod.hiddenNames().get().map(Literal::name);
            symbolsToIterate = exportedSymbolNames
                .filter(exportedSymName -> !hiddenNames.contains(exportedSymName))
                .toList();
          } else {
            symbolsToIterate = exportedSymbolNames.toList();
          }
          for (var symbolName : symbolsToIterate) {
            for (var importTarget : importTargets) {
              var resolution = importTarget.resolveExportedSymbol(symbolName);
              if (resolution.isLeft()) {
                throw new CompilerError(
                    "Unreachable: (should have been resolved in previous passes): " + resolution);
              }
              var resolvedNames = resolution.toOption().get();
              if (resolvedNames.size() > 1) {
                // If the symbolName is resolved to multiple objects, we ignore it.
                continue;
              }
              var resolvedName = resolvedNames.head();
              var symbolPath = resolvedName.qualifiedName().toString();
              tryAddEncounteredSymbol(impMod, symbolName, symbolPath, resolvedName, errorsForImport);
            }
          }
        }

        // Import a renamed symbol
        case Import.Module impMod when impMod.rename().isDefined() -> {
          var symbolPath = impMod.name().name();
          var symbolName = impMod.rename().get().name();
          tryAddEncounteredSymbol(impMod, symbolName, symbolPath, null, errorsForImport);
        }

        // Import one symbol
        case Import.Module impMod when !impMod.isSynthetic() -> {
          var symbolPath = impMod.name().name();
          var symbolName = impMod.name().parts().last().name();
          tryAddEncounteredSymbol(impMod, symbolName, symbolPath, null, errorsForImport);
        }

        case Polyglot polyglotImp -> {
          var symbolName = polyglotImp.rename().getOrElse(() -> polyglotImp.entity().getVisibleName());
          String symbolPath;
          if (polyglotImp.entity() instanceof Polyglot.Java javaEntity) {
            symbolPath = javaEntity.packageName() + "." + javaEntity.className();
          } else {
            throw new IllegalStateException("Unsupported polyglot entity: " + polyglotImp.entity());
          }
          tryAddEncounteredSymbol(polyglotImp, symbolName, symbolPath, null, errorsForImport);
        }

        default -> {}
      }
      return errorsForImport;
    }

    /**
     * Tries to add the encountered symbol to the encountered symbols map. If it is already contained
     * in the map, checks whether the underlying entity path is the same as the original entity path.
     * Based on that, either attaches a warning for a duplicated import, or returns an {@link ImportExport}.
     *
     * @param currentImport Currently iterated import
     * @param symbolName Name of the symbol that is about to be processed
     * @param symbolPath physical path of the symbol that is about to be processed
     * @param errors Into this list, a potential error is appended.
     */
    private void tryAddEncounteredSymbol(
        Import currentImport,
        String symbolName,
        String symbolPath,
        ResolvedName resolvedName,
        List<ImportExport> errors) {
      if (!encounteredSymbols.containsSymbol(symbolName)) {
        encounteredSymbols.addSymbol(currentImport, symbolName, symbolPath, resolvedName);
      } else {
        var encounteredFullName = encounteredSymbols.getPathForSymbol(symbolName);
        var originalImport = encounteredSymbols.getOriginalImportForSymbol(symbolName);
        if (symbolPath.equals(encounteredFullName)) {
          // symbolName is already imported with the same symbolPath --> attach warning.
          var warn = createWarningForDuplicatedImport(originalImport, currentImport, symbolName);
          currentImport.getDiagnostics().add(warn);
        } else {
          // There is an encountered symbol with different physical path than symbolPath.
          var resolution = encounteredSymbols.getResolvedNameForSymbol(symbolName);
          if (resolution instanceof BindingsMap.ResolvedMethod resMethod &&
              resMethod.methodName().equals(symbolName)) {
            // This is a valid ambiguous case - in previously encountered import, the symbol was resolved
            // to either an extension, static, or conversion method.
            return;
          } else {
            var error = createErrorForAmbiguousImport(
                originalImport,
                encounteredFullName,
                currentImport,
                symbolName,
                symbolPath);
            errors.add(error);
          }
        }
      }
    }

    private static Warning createWarningForDuplicatedImport(
        Import originalImport,
        Import duplicatingImport,
        String duplicatedSymbol
    ) {
      return new Warning.DuplicatedImport(
          duplicatingImport.identifiedLocation(),
          originalImport,
          duplicatedSymbol
      );
    }

    private ImportExport createErrorForAmbiguousImport(
        Import originalImport,
        String originalSymbolPath,
        Import duplicatingImport,
        String ambiguousSymbol,
        String ambiguousSymbolPath) {
      return ImportExport.apply(
          duplicatingImport,
          new ImportExport.AmbiguousImport(
              originalImport,
              originalSymbolPath,
              ambiguousSymbol,
              ambiguousSymbolPath
          ),
          new MetadataStorage()
      );
    }

    private List<ImportTarget> getImportTargets(Import imp) {
      var found = bindingsMap.resolvedImports().find(resImp -> resImp.importDef() == imp);
      if (found.isDefined()) {
        return CollectionConverters.asJava(found.get().targets());
      } else {
        return List.of();
      }
    }
  }

  /** @param symbolPath Fully qualified name of the symbol, i.e., its physical path.
   * @param resolvedName The optional resolved name of the symbol.
   * @param originalImport The import IR from which the symbol was originally imported.
   *                       i.e. the first encountered import IR that imports the symbol.
   */
  private record SymbolTarget(
      String symbolPath,
      ResolvedName resolvedName,
      Import originalImport
  ) {
    private SymbolTarget {
      assert symbolPath != null;
      assert originalImport != null;
    }
  }


  /** For every encountered symbol name, we keep track of the original import from which it was imported,
   * along with the entity path. The entity path is vital to decide whether an imported symbol is duplicated
   * or ambiguous.
   * Note that there are some exceptions that are allowed to be ambiguous, like extension methods.
   */
  private static final class EncounteredSymbols {
    private final Map<String, SymbolTarget> symbols = new HashMap<>();

    boolean containsSymbol(String symbolName) {
      return symbols.containsKey(symbolName);
    }

    void addSymbol(Import imp, String symbol, String symbolPath, ResolvedName resolvedName) {
      symbols.put(symbol, new SymbolTarget(symbolPath, resolvedName, imp));
    }

    String getPathForSymbol(String symbol) {
      return symbols.get(symbol).symbolPath;
    }

    ResolvedName getResolvedNameForSymbol(String symbol) {
      return symbols.get(symbol).resolvedName;
    }

    Import getOriginalImportForSymbol(String symbol) {
      var val = symbols.get(symbol);
      if (val != null) {
        return val.originalImport;
      } else {
        return null;
      }
    }
  }
}
