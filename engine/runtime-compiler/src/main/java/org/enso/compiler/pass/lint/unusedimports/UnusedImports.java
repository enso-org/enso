package org.enso.compiler.pass.lint.unusedimports;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.analyse.AmbiguousImportsAnalysis;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.analyse.ImportSymbolAnalysis;
import org.enso.compiler.pass.resolve.GenericAnnotations$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.MethodDefinitions;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.pkg.QualifiedName;
import org.enso.scala.wrapper.ScalaConversions;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Attaches warnings to those {@link Import import IRs} that export symbols that are not used in any
 * expression in the current module.
 *
 * <p>Works with then {@link BindingsMap.Resolution} metadata from various passes.
 *
 * <p>Traverses the entire {@link Module module IR}, looks for all the {@link BindingsMap.Resolution
 * resolved} symbols, and assigns these resolutions to their corresponding {@link Import import IR}.
 *
 * <p>Intentionally not implemented as {@link org.enso.compiler.pass.MiniIRPass mini pass}.
 *
 * <p>This is an expensive pass and should be run only during linting.
 */
public final class UnusedImports implements IRPass {
  public static final UnusedImports INSTANCE = new UnusedImports();

  private UnusedImports() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(
            BindingAnalysis$.MODULE$,
            ImportSymbolAnalysis.INSTANCE,
            AmbiguousImportsAnalysis.INSTANCE,
            TypeNames$.MODULE$,
            TypeSignatures$.MODULE$,
            MethodDefinitions.INSTANCE,
            GlobalNames$.MODULE$,
            Patterns$.MODULE$,
            GenericAnnotations$.MODULE$);
    return ScalaConversions.seq(passes);
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    return ScalaConversions.seq(List.of());
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    var bm = moduleContext.bindingsAnalysis();
    var usedSymbols = UsedSymbolsCollector.collect(ir, bm);
    for (var imp : CollectionConverters.asJava(ir.imports())) {
      if (!shouldImportBeSkipped(imp)) {
        if (imp instanceof Import.Module impMod && impMod.onlyNames().isDefined()) {
          var importedSymbols = importedSymbols(imp, bm);
          var usedSymbolsForImp = usedSymbols.getUsedSymbolsForImport(imp);
          var diff = new HashSet<>(importedSymbols);
          diff.removeAll(usedSymbolsForImp);
          if (!diff.isEmpty()) {
            var warn = createWarning(imp, diff);
            imp.getDiagnostics().add(warn);
          }
        } else {
          var usedSymbolsForImp = usedSymbols.getUsedSymbolsForImport(imp);
          if (usedSymbolsForImp.isEmpty()) {
            var warn = createWarning(imp);
            imp.getDiagnostics().add(warn);
          }
        }
      }
    }
    return ir;
  }

  /** Returns set of all imported symbol by the given import statement. */
  private Set<QualifiedName> importedSymbols(Import impIr, BindingsMap bindingsMap) {
    var resolvedImp = findResolvedImport(impIr, bindingsMap);
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

  private static BindingsMap.ResolvedImport findResolvedImport(
      Import impIr, BindingsMap bindingsMap) {
    for (var resolvedImp : CollectionConverters.asJava(bindingsMap.resolvedImports())) {
      if (resolvedImp.importDef() == impIr || haveSameLocations(resolvedImp.importDef(), impIr)) {
        return resolvedImp;
      }
    }
    return null;
  }

  static boolean haveSameLocations(Import imp1, Import imp2) {
    var loc1 = imp1.identifiedLocation();
    var loc2 = imp2.identifiedLocation();
    if (loc1 != null && loc2 != null) {
      return loc1.start() == loc2.start() && loc1.end() == loc2.end();
    }
    return false;
  }

  private static boolean shouldImportBeSkipped(Import imp) {
    return isImportDuplicated(imp) || imp instanceof Polyglot || isAllImport(imp);
  }

  private static boolean isAllImport(Import imp) {
    return imp instanceof Import.Module impMode
        && impMode.isAll()
        && !impMode.onlyNames().isDefined();
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
}
