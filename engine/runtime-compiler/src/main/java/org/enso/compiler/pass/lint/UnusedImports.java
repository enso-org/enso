package org.enso.compiler.pass.lint;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.Warning.UnusedImport;
import org.enso.compiler.core.ir.Warning.UnusedSymbolsFromImport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.analyse.AmbiguousImportsAnalysis;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.analyse.ImportSymbolAnalysis;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.pkg.QualifiedName;
import org.enso.scala.wrapper.ScalaConversions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Attaches warnings to those {@link Import import IRs} that export symbols that are not used in any
 * expression in the current module.
 *
 * <p>Works with the {@link BindingsMap.Resolution} metadata from {@link
 * org.enso.compiler.pass.resolve.GlobalNames} pass.
 */
public final class UnusedImports implements MiniPassFactory {
  private static final Logger LOGGER = LoggerFactory.getLogger(UnusedImports.class);
  public static final UnusedImports INSTANCE = new UnusedImports();

  private UnusedImports() {}

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    var bm = moduleContext.bindingsAnalysis();
    return new Mini(bm);
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }

  @Override
  public Seq<? extends IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(
            BindingAnalysis$.MODULE$,
            ImportSymbolAnalysis.INSTANCE,
            AmbiguousImportsAnalysis.INSTANCE,
            GlobalNames$.MODULE$);
    return ScalaConversions.seq(passes);
  }

  @Override
  public Seq<? extends IRProcessingPass> invalidatedPasses() {
    return ScalaConversions.seq(List.of());
  }

  private static final class Mini extends MiniIRPass {
    private final BindingsMap bindingsMap;
    private final UsedSymbols.Builder usedSymbolsBldr = new UsedSymbols.Builder();

    private Mini(BindingsMap bindingsMap) {
      this.bindingsMap = bindingsMap;
    }

    @Override
    public MiniIRPass prepare(IR parent, Expression child) {
      var resolutionMeta =
          MetadataInteropHelpers.getMetadataOrNull(
              child, GlobalNames$.MODULE$, BindingsMap.Resolution.class);
      LOGGER.trace(
          "[{}] Preparing for parent={}, child={}, resolutionMeta={}",
          bindingsMap.currentModule().getName(),
          parent.getClass().getName(),
          child.getClass().getName(),
          resolutionMeta);
      if (resolutionMeta != null) {
        var targetMod = resolutionMeta.target().module();
        var targetModName = targetMod.getName();
        var targetSymbolName = resolutionMeta.target().qualifiedName();
        var imports = findImportIRs(targetModName, targetSymbolName);
        for (var imp : imports) {
          LOGGER.trace(
              "[{}] Adding used symbol '{}' for import '{}'",
              bindingsMap.currentModule().getName(),
              targetSymbolName,
              imp.showCode());
          usedSymbolsBldr.addUsedSymbol(imp, targetSymbolName);
        }
      }
      return this;
    }

    /**
     * Finds import IRs that import the given symbol from the given module. Note that a symbol may
     * be imported by multiple import IRs.
     *
     * @param targetModName Name of the module that should exports the symbol
     * @param targetSymbolName
     * @return
     */
    private List<Import.Module> findImportIRs(
        QualifiedName targetModName, QualifiedName targetSymbolName) {
      var importDefs = new ArrayList<Import.Module>();
      for (var resolvedImp : CollectionConverters.asJava(bindingsMap.resolvedImports())) {
        var impIR = resolvedImp.importDef();
        if (impIR.onlyNames().isDefined() || impIR.isAll()) {
          // `onlyNames`, or `isAll` import usually has a single resolved import, with target of the
          // ResolvedModule
          var resolvedMod =
              resolvedImp.targets().find(target -> target instanceof BindingsMap.ResolvedModule);
          if (resolvedMod.isDefined()) {
            var resolvedNames = resolvedMod.get().findExportedSymbolsFor(targetSymbolName.item());
            var exportsSymbol = !resolvedNames.isEmpty();
            if (exportsSymbol) {
              importDefs.add(impIR);
            }
          }
        } else {
          var hasSymbolInTargets =
              resolvedImp
                  .targets()
                  .exists(target -> target.qualifiedName().equals(targetSymbolName));
          if (hasSymbolInTargets) {
            importDefs.add(impIR);
          }
        }
      }
      LOGGER.trace(
          "[{}] Found import IRs for module '{}' and symbol '{}': {}",
          bindingsMap.currentModule().getName(),
          targetModName,
          targetSymbolName,
          importDefsToString(importDefs));
      return importDefs;
    }

    @Override
    public Module transformModule(Module moduleIr) {
      gatherUsedSymbolsFromExports(moduleIr);
      var usedSymbols = usedSymbolsBldr.build();
      LOGGER.trace(
          "[{}] Transforming module. Used symbols: {}",
          bindingsMap.currentModule().getName(),
          usedSymbols);
      var newImports = new ArrayList<Import>();
      for (var impIr : CollectionConverters.asJava(moduleIr.imports())) {
        if (isImportDuplicated(impIr)) {
          // nop
        } else if (impIr instanceof Import.Module impMod && impMod.onlyNames().isDefined()) {
          var importedSymbols = importedSymbols(impIr);
          var usedSymbolsForImp = usedSymbols.getUsedSymbolsForImport(impIr);
          var diff = new HashSet<>(importedSymbols);
          diff.removeAll(usedSymbolsForImp);
          if (!diff.isEmpty()) {
            var warn = createWarning(impIr, diff);
            LOGGER.trace(
                "[{}] Adding warning for unused symbols: {} to import '{}'",
                bindingsMap.currentModule().getName(),
                diff,
                impIr.showCode());
            impIr.getDiagnostics().add(warn);
          }
        } else {
          var usedSymbolsForImp = usedSymbols.getUsedSymbolsForImport(impIr);
          if (usedSymbolsForImp.isEmpty()) {
            var warn = createWarning(impIr);
            LOGGER.trace(
                "[{}] Adding warning for unused import to '{}'",
                bindingsMap.currentModule().getName(),
                impIr.showCode());
            impIr.getDiagnostics().add(warn);
          }
        }
        newImports.add(impIr);
      }
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

    @Override
    public Expression transformExpression(Expression expr) {
      return expr;
    }

    /**
     * Traverses export IRs and fills in {@link #usedSymbolsBldr} based on the exported symbols.
     * Note that the export IRs is not traversed by {@link #prepare(IR, Expression)}.
     */
    private void gatherUsedSymbolsFromExports(Module modIr) {
      LOGGER.trace(
          "[{}] Gathering used symbols from exports", bindingsMap.currentModule().getName());
      for (var export : exports(modIr)) {
        if (export.onlyNames().isDefined()) {
          var names = export.onlyNames().get().map(Literal::name);
          var modName = QualifiedName.fromString(export.name().name());
          for (var name : CollectionConverters.asJava(names)) {
            var symName =
                QualifiedName.fromString(modName.toString() + QualifiedName.separator() + name);
            var impIrs = findImportIRs(modName, symName);
            for (var impIr : impIrs) {
              LOGGER.trace(
                  "[{}] Adding used symbol '{}' for import '{}' from export '{}'",
                  bindingsMap.currentModule().getName(),
                  symName,
                  impIr.showCode(),
                  export.showCode());
              usedSymbolsBldr.addUsedSymbol(impIr, symName);
            }
          }
        } else {
          var simpleName = export.getSimpleName().name();
          var resolvedNames = resolveExportedName(simpleName);
          addToUsedSymbols(resolvedNames);
        }
      }
    }

    private List<ResolvedName> resolveExportedName(String name) {
      var resolution = bindingsMap.resolveExportedName(name);
      if (resolution.isLeft()) {
        throw new AssertionError(
            "The name '"
                + name
                + "' should be in exported symbols in bindings map: "
                + bindingsMap.exportedSymbols());
      }
      var resolvedNames = resolution.toOption().get();
      return CollectionConverters.asJava(resolvedNames);
    }

    private void addToUsedSymbols(List<ResolvedName> resolvedNames) {
      for (var resolvedName : resolvedNames) {
        var impIRs = findImportIRs(resolvedName);
        for (var impIR : impIRs) {
          var symName = resolvedName.qualifiedName();
          LOGGER.trace(
              "[{}] Adding used symbol '{}' for import '{}'",
              bindingsMap.currentModule().getName(),
              symName,
              impIR.showCode());
          usedSymbolsBldr.addUsedSymbol(impIR, symName);
        }
      }
    }

    private List<Import.Module> findImportIRs(ResolvedName resolvedName) {
      var modName = resolvedName.module().getName();
      var symName = resolvedName.qualifiedName();
      return findImportIRs(modName, symName);
    }

    private static List<Export.Module> exports(Module modIr) {
      var exps =
          modIr
              .exports()
              .map(
                  exp -> {
                    assert exp instanceof Export.Module
                        : "Only single subtype of Import is implemented";
                    return (Export.Module) exp;
                  });
      return CollectionConverters.asJava(exps);
    }

    /** Returns set of all imported symbol by the given import statement. */
    private Set<QualifiedName> importedSymbols(Import impIr) {
      var resolvedImp = findResolvedImport(impIr);
      if (resolvedImp == null) {
        var errMsg = new StringBuilder();
        errMsg
            .append("No resolved import found for import statement '")
            .append(impIr.showCode())
            .append("' in module '")
            .append(bindingsMap.currentModule().getName())
            .append("'");
        var loc = impIr.identifiedLocation();
        if (loc != null) {
          errMsg.append("at location: {").append(loc).append("}");
        }
        errMsg.append(".");
        throw new CompilerError(errMsg.toString());
      }
      var symbols = importedSymbols(resolvedImp);
      return new HashSet<>(symbols);
    }

    private static List<QualifiedName> importedSymbols(BindingsMap.ResolvedImport resolvedImport) {
      var impDef = resolvedImport.importDef();
      if (impDef.onlyNames().isDefined()) {
        var targets = resolvedImport.targets();
        if (targets.size() != 1) {
          throw new AssertionError(
              "Resolved import for '"
                  + impDef.showCode()
                  + "' should have a single target."
                  + " Instead, targets are: "
                  + resolvedImport.targets());
        }
        var target = targets.head();
        var names = impDef.onlyNames().get().map(Literal::name);
        var resolvedNames = new ArrayList<QualifiedName>();
        names.foreach(
            name -> {
              var expSymbols = target.findExportedSymbolsFor(name);
              expSymbols.foreach(
                  expSymbol -> {
                    resolvedNames.add(expSymbol.qualifiedName());
                    return null;
                  });
              return null;
            });
        return resolvedNames;
      } else {
        var names = resolvedImport.targets().map(ResolvedName::qualifiedName);
        return CollectionConverters.asJava(names);
      }
    }

    private BindingsMap.ResolvedImport findResolvedImport(Import impIr) {
      for (var resolvedImp : CollectionConverters.asJava(bindingsMap.resolvedImports())) {
        if (resolvedImp.importDef() == impIr || haveSameLocations(resolvedImp.importDef(), impIr)) {
          return resolvedImp;
        }
      }
      return null;
    }

    private static boolean haveSameLocations(Import imp1, Import imp2) {
      var loc1 = imp1.identifiedLocation();
      var loc2 = imp2.identifiedLocation();
      if (loc1 != null && loc2 != null) {
        return loc1.start() == loc2.start() && loc1.end() == loc2.end();
      }
      return false;
    }

    private static boolean isImportDuplicated(Import imp) {
      if (imp.diagnostics() != null) {
        var duplImport =
            imp.diagnostics().toList().find(diag -> diag instanceof Warning.DuplicatedImport);
        return duplImport.isDefined();
      } else {
        return false;
      }
    }

    private static UnusedSymbolsFromImport createWarning(
        Import impIr, Set<QualifiedName> unusedSymbols) {
      var list = unusedSymbols.stream().map(QualifiedName::toString).sorted().toList();
      return new UnusedSymbolsFromImport(
          impIr.identifiedLocation(), CollectionConverters.asScala(list).toList());
    }

    private static UnusedImport createWarning(Import impIr) {
      var loc = impIr.identifiedLocation();
      return new UnusedImport(loc);
    }

    private static String importDefsToString(List<Import.Module> imps) {
      var str =
          imps.stream().map(imp -> "'" + imp.showCode() + "'").collect(Collectors.joining(", "));
      return "[" + str + "]";
    }
  }

  /** All the used symbols inside one module. */
  private static final class UsedSymbols {
    private final Map<Import, Set<QualifiedName>> symbols;

    private UsedSymbols(Map<Import, Set<QualifiedName>> symbols) {
      this.symbols = symbols;
    }

    private Set<QualifiedName> getUsedSymbolsForImport(Import importIr) {
      return symbols.getOrDefault(importIr, Set.of());
    }

    @Override
    public String toString() {
      var sb = new StringBuilder();
      sb.append("UsedSymbols{");
      for (var entry : symbols.entrySet()) {
        var impCode = entry.getKey().showCode();
        sb.append("'").append(impCode).append("': ").append(entry.getValue()).append(", ");
      }
      sb.append("}");
      return sb.toString();
    }

    private static final class Builder {
      private final Map<Import, Set<QualifiedName>> symbols = new HashMap<>();

      void addUsedSymbol(Import importIr, QualifiedName symbol) {
        var usedSymbols = symbols.computeIfAbsent(importIr, k -> new HashSet<>());
        usedSymbols.add(symbol);
      }

      UsedSymbols build() {
        return new UsedSymbols(Collections.unmodifiableMap(symbols));
      }
    }
  }
}
