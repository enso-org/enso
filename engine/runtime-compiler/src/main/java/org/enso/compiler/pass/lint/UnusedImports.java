package org.enso.compiler.pass.lint;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Warning.UnusedImport;
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
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Attaches warnings to those {@link Import import IRs} that export symbols that are not used in any
 * expression in the current module. TODO: Ignore duplicated imports?
 */
public final class UnusedImports implements MiniPassFactory {
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
      if (resolutionMeta != null) {
        var targetMod = resolutionMeta.target().module();
        var targetModName = targetMod.getName();
        var targetSymbolName = resolutionMeta.target().qualifiedName();
        var modImports = findImportsOfModule(targetModName);
        for (var imp : modImports) {
          usedSymbolsBldr.addUsedSymbol(imp, targetSymbolName);
        }
      }
      return this;
    }

    @Override
    public Module transformModule(Module moduleIr) {
      var usedSymbols = usedSymbolsBldr.build();
      var newImports = new ArrayList<Import>();
      for (var impIr : CollectionConverters.asJava(moduleIr.imports())) {
        var importedSymbols = importedSymbols(impIr);
        var usedSymbolsForImp = usedSymbols.getUsedSymbolsForImport(impIr);
        var diff = new HashSet<>(importedSymbols);
        diff.removeAll(usedSymbolsForImp);
        if (!diff.isEmpty()) {
          var warn = createWarning(impIr, diff);
          impIr.getDiagnostics().add(warn);
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

    private static UnusedImport createWarning(Import impIr, Set<QualifiedName> unusedSymbols) {
      var list = unusedSymbols.stream().map(QualifiedName::toString).sorted().toList();
      return new UnusedImport(
          impIr.identifiedLocation(), CollectionConverters.asScala(list).toList());
    }

    @Override
    public Expression transformExpression(Expression expr) {
      return expr;
    }

    /**
     * Finds all Import IR definitions for the module of given name. Note that a module may be
     * imported by multiple import statements.
     *
     * @param modName
     * @return Non empty list. Not null.
     */
    private List<Import.Module> findImportsOfModule(QualifiedName modName) {
      var importDefs = new ArrayList<Import.Module>();
      for (var resolvedImp : CollectionConverters.asJava(bindingsMap.resolvedImports())) {
        var modNames = importedModules(resolvedImp);
        if (modNames.contains(modName)) {
          importDefs.add(resolvedImp.importDef());
        }
      }
      return importDefs;
    }

    /**
     * Returns list of module names that are imported by the given resolved import.
     *
     * @param resolvedImport
     * @return non-empty list of module names.
     */
    private static List<QualifiedName> importedModules(BindingsMap.ResolvedImport resolvedImport) {
      var names = resolvedImport.targets().map(target -> target.module().getName());
      return CollectionConverters.asJava(names);
    }

    private static List<QualifiedName> importedSymbols(BindingsMap.ResolvedImport resolvedImport) {
      if (resolvedImport.importDef().onlyNames().isDefined()) {
        var entityName = resolvedImport.importDef().name().name();
        var names = resolvedImport.importDef().onlyNames().get().map(Literal::name);
        var qualifiedNames =
            names.map(nm -> QualifiedName.fromString(entityName + QualifiedName.separator() + nm));
        return CollectionConverters.asJava(qualifiedNames);
      } else {
        var names = resolvedImport.targets().map(ResolvedName::qualifiedName);
        return CollectionConverters.asJava(names);
      }
    }

    /**
     * Returns set of all imported symbol by the given import statement.
     *
     * @param impIr
     * @return
     */
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

    private BindingsMap.ResolvedImport findResolvedImport(Import impIr) {
      for (var resolvedImp : CollectionConverters.asJava(bindingsMap.resolvedImports())) {
        if (resolvedImp.importDef() == impIr) {
          return resolvedImp;
        }
      }
      return null;
    }
  }

  /** All the used symbols inside one module. */
  private static final class UsedSymbols {
    private final Map<Import, Set<QualifiedName>> symbols;

    private UsedSymbols(Map<Import, Set<QualifiedName>> symbols) {
      this.symbols = symbols;
    }

    private Set<QualifiedName> getUsedSymbolsForImport(Import importIr) {
      var usedSymbols = symbols.get(importIr);
      assert usedSymbols != null;
      return usedSymbols;
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
