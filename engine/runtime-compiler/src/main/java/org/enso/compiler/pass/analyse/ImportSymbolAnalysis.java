package org.enso.compiler.pass.analyse;

import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.desugar.GenerateMethodBodies$;
import org.enso.scala.wrapper.ScalaConversions;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Performs analysis of `from ... import sym1, sym2, ...` statements - checks that all the symbols
 * imported from the module can be resolved, i.e., exists. In case of unresolved symbols, replaces
 * the IR import with {@link org.enso.compiler.core.ir.expression.errors.ImportExport}. Reports only
 * the first unresolved symbol.
 */
public final class ImportSymbolAnalysis implements MiniPassFactory {
  public static final ImportSymbolAnalysis INSTANCE = new ImportSymbolAnalysis();

  private ImportSymbolAnalysis() {}

  @Override
  public Seq<? extends IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(BindingAnalysis$.MODULE$, GenerateMethodBodies$.MODULE$);
    return ScalaConversions.seq(passes);
  }

  @Override
  public Seq<? extends IRProcessingPass> invalidatedPasses() {
    return ScalaConversions.seq(List.of());
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    var bindingsMap = moduleContext.bindingsAnalysis();
    assert bindingsMap != null;
    return new Mini(bindingsMap);
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    // Does not make sense for inline compilation.
    return null;
  }

  private static final class Mini extends MiniIRPass {
    private final BindingsMap bindingsMap;

    private Mini(BindingsMap bindingsMap) {
      this.bindingsMap = bindingsMap;
    }

    @Override
    public Module transformModule(Module moduleIr) {
      var newImports = new ArrayList<Import>();
      for (var imp : CollectionConverters.asJava(moduleIr.imports())) {
        if (imp instanceof Import.Module modImp) {
          var encounteredErrors = analyseSymbolsFromImport(modImp);
          if (encounteredErrors != null) {
            newImports.addAll(encounteredErrors);
            continue;
          }
        }
        newImports.add(imp);
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
    public MiniIRPass prepare(IR parent, Expression child) {
      // return null - do not traverse any children of the root - we just
      // need to transform the module IR.
      return null;
    }

    @Override
    public Expression transformExpression(Expression expr) {
      throw new IllegalStateException("Should not be called - prepare returns null");
    }

    /** Returns list of encountered errors, or null. */
    private List<Import> analyseSymbolsFromImport(Import.Module imp) {
      if (imp.onlyNames().isDefined()) {
        var resolvedImport =
            bindingsMap.resolvedImports().find(resImp -> resImp.importDef() == imp);
        if (resolvedImport.isEmpty()) {
          return null;
        }
        var onlyNames = imp.onlyNames().get();
        var importedTargets = resolvedImport.get().targets();
        var unresolvedSymbols =
            importedTargets.flatMap(
                importedTarget -> onlyNames.filterNot(nm -> isSymbolResolved(importedTarget, nm)));
        if (unresolvedSymbols.nonEmpty()) {
          scala.collection.immutable.List<Import> errs =
              unresolvedSymbols.map(
                  unresolvedSym ->
                      createErrorForUnresolvedSymbol(imp, importedTargets.head(), unresolvedSym));
          return CollectionConverters.asJava(errs);
        }
      }

      // Importing symbols from methods is not allowed. The following code checks that if the
      // import is importing all from a method, an error is reported.
      if (imp.isAll() && !imp.isSynthetic()) {
        var resolvedImport =
            bindingsMap.resolvedImports().find(resImp -> resImp.importDef() == imp);
        if (resolvedImport.isEmpty()) {
          return null;
        }
        var importedTargets = resolvedImport.get().targets();
        var encounteredErrors = new ArrayList<Import>();
        for (var importedTarget : CollectionConverters.asJava(importedTargets)) {
          switch (importedTarget) {
            case BindingsMap.ResolvedModuleMethod resModMethod -> {
              encounteredErrors.add(
                  createImportFromMethodError(
                      imp,
                      resModMethod.module().getName().toString(),
                      resModMethod.method().name()));
            }
            case BindingsMap.ResolvedExtensionMethod extMethod -> {
              var staticMethod = extMethod.staticMethod();
              encounteredErrors.add(
                  createImportFromMethodError(
                      imp,
                      extMethod.module().getName().createChild(staticMethod.tpName()).toString(),
                      staticMethod.methodName()));
            }
            case BindingsMap.ResolvedConversionMethod resConvMethod -> {
              var convMethod = resConvMethod.conversionMethod();
              var module = resConvMethod.module();
              encounteredErrors.add(
                  createImportFromMethodError(
                      imp,
                      module.getName().createChild(convMethod.targetTpName()).toString(),
                      convMethod.methodName()));
            }
            default -> {}
          }
        }
        if (!encounteredErrors.isEmpty()) {
          return encounteredErrors;
        }
      }
      return null;
    }

    private static boolean isSymbolResolved(
        BindingsMap.ImportTarget importTarget, Name.Literal symbol) {
      return importTarget.findExportedSymbolsFor(symbol.name()).nonEmpty();
    }

    private static ImportExport createErrorForUnresolvedSymbol(
        Import imp, BindingsMap.ImportTarget importTarget, Name.Literal unresolvedSymbol) {
      ImportExport.Reason errorReason =
          switch (importTarget) {
            case BindingsMap.ResolvedModule resMod -> new ImportExport.SymbolDoesNotExist(
                unresolvedSymbol.name(), resMod.module().getName().toString());
            case BindingsMap.ResolvedType resType -> new ImportExport.NoSuchConstructor(
                resType.tp().name(), unresolvedSymbol.name());
            case BindingsMap.ResolvedConstructor resCons -> new ImportExport.NoSuchConstructor(
                resCons.cons().name(), unresolvedSymbol.name());
            case BindingsMap.ResolvedModuleMethod resMethod -> new ImportExport.NoSuchModuleMethod(
                resMethod.method().name(), unresolvedSymbol.name());
            case BindingsMap.ResolvedExtensionMethod extMethod -> new ImportExport
                .NoSuchStaticMethod(
                extMethod.module().getName().toString(),
                extMethod.staticMethod().tpName(),
                unresolvedSymbol.name());
            case BindingsMap.ResolvedConversionMethod convMethod -> new ImportExport
                .NoSuchConversionMethod(
                convMethod.module().getName().toString(),
                convMethod.conversionMethod().targetTpName(),
                convMethod.conversionMethod().sourceTpName());
            default -> throw new IllegalStateException("Unexpected value: " + importTarget);
          };
      return new ImportExport(imp, errorReason, new MetadataStorage());
    }

    private static ImportExport createImportFromMethodError(
        Import imp, String moduleName, String methodName) {
      return new ImportExport(
          imp,
          new ImportExport.IllegalImportFromMethod(moduleName, methodName),
          new MetadataStorage());
    }
  }
}
