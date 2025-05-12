package org.enso.compiler.pass.lint.unusedimports;

import static scala.jdk.javaapi.CollectionConverters.asJava;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.Resolution;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.lint.unusedimports.UsedSymbols.Builder;
import org.enso.compiler.pass.resolve.GenericAnnotations$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.MethodDefinitions;
import org.enso.compiler.pass.resolve.ModuleAnnotations;
import org.enso.compiler.pass.resolve.ModuleAnnotations.Annotations;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.pkg.QualifiedName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Traverses the entire {@link Module module IR}, looks for all the {@link BindingsMap.Resolution
 * resolved} symbols, and assigns these resolutions to their corresponding {@link Import import IR}.
 *
 * <p>Not implemented as a {@link org.enso.compiler.pass.IRPass compiler pass} on purpose. The
 * caller is responsible that all the required metadata is present in the IR.
 */
final class UsedSymbolsCollector {
  private final Module moduleIr;
  private final BindingsMap bindingsMap;
  private final UsedSymbols.Builder usedSymbolsBldr;
  private static final Logger LOGGER = LoggerFactory.getLogger(UsedSymbolsCollector.class);

  private UsedSymbolsCollector(Module moduleIr, BindingsMap bindingsMap) {
    this.moduleIr = moduleIr;
    this.bindingsMap = bindingsMap;
    this.usedSymbolsBldr = new Builder();
  }

  static UsedSymbols collect(Module moduleIr, BindingsMap bindingsMap) {
    var collector = new UsedSymbolsCollector(moduleIr, bindingsMap);
    return collector.collect();
  }

  private UsedSymbols collect() {
    gatherUsedSymbolsFromExports(moduleIr);
    collectFromRoot(moduleIr);
    var usedSymbols = usedSymbolsBldr.build();
    return usedSymbols;
  }

  /**
   * Traverses the whole subtree of the given {@code ir} and collects all the {@link Resolution}
   * metadata. Note that this is an expensive method.
   */
  private void collectFromRoot(IR root) {
    var irsToProcess = new ArrayDeque<IR>();
    irsToProcess.add(root);
    while (!irsToProcess.isEmpty()) {
      var ir = irsToProcess.removeFirst();
      // Application.Prefix (method calls) are handled specifically. GlobalNames pass assigns
      // resolution to the first synthetic self argument.
      if (ir instanceof Application.Prefix app
          && app.function() instanceof Name.Literal funcLiteral) {
        if (!app.arguments().isEmpty()) {
          var selfArg = app.arguments().head();
          var selfArgResolution =
              MetadataInteropHelpers.getMetadataOrNull(
                  selfArg.value(), GlobalNames$.MODULE$, BindingsMap.Resolution.class);
          if (selfArgResolution != null) {
            var targetModName = selfArgResolution.target().module().getName();
            QualifiedName targetSymbolName;
            switch (selfArgResolution.target()) {
              case BindingsMap.ResolvedType tp -> {
                targetSymbolName = targetModName.createChild(tp.tp().name());
              }
              case BindingsMap.ResolvedMethod method -> {
                targetSymbolName = targetModName.createChild(method.methodName());
              }
              case BindingsMap.ResolvedModule module -> {
                // This is most likely call of a module method
                targetSymbolName = targetModName.createChild(funcLiteral.name());
              }
              default -> {
                targetSymbolName = selfArgResolution.target().qualifiedName();
              }
            }
            addUsedSymbol(targetModName, targetSymbolName);
            // Add all the children except for the first argument
            asJava(app.arguments()).stream().skip(1).forEach(irsToProcess::addLast);
            irsToProcess.addLast(app.function());
          } else {
            irsToProcess.addAll(asJava(ir.children()));
          }
        } else {
          irsToProcess.addAll(asJava(ir.children()));
        }
      } else {
        addUsedSymbolForResolution(getGlobalNamesMeta(ir));
        addUsedSymbolForResolution(getMethodDefinitionsMeta(ir));
        addUsedSymbolForResolution(getTypeNameMeta(ir));
        addUsedSymbolForResolution(getPatternsMeta(ir));
        var typeSig = getTypeSignatureMeta(ir);
        if (typeSig != null) {
          var sig = typeSig.signature();
          irsToProcess.addLast(sig);
        }
        var anotMeta = getGenericAnnotationMeta(ir);
        if (anotMeta != null) {
          var annotations = asJava(anotMeta.annotations());
          irsToProcess.addAll(annotations);
        }
        var children = asJava(ir.children());
        irsToProcess.addAll(children);
      }
    }
  }

  /** Traverses export IRs and fills in {@link #usedSymbolsBldr} based on the exported symbols. */
  private void gatherUsedSymbolsFromExports(Module modIr) {
    LOGGER.trace("[{}] Gathering used symbols from exports", bindingsMap.currentModule().getName());
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

  /**
   * Finds import IRs that import the given symbol from the given module. Note that a symbol may be
   * imported by multiple import IRs.
   *
   * @param targetModName Name of the module that should exports the symbol
   * @param targetSymbolName
   * @return
   */
  private List<Import.Module> findImportIRs(
      QualifiedName targetModName, QualifiedName targetSymbolName) {
    var importDefs = new ArrayList<Import.Module>();
    for (var resolvedImp : asJava(bindingsMap.resolvedImports())) {
      var impIR = resolvedImp.importDef();
      var validTarget =
          resolvedImp
              .targets()
              .find(
                  target -> {
                    var resolvedNames = target.findExportedSymbolsFor(targetSymbolName.item());
                    var targetNameMatches = target.qualifiedName().equals(targetSymbolName);
                    var exportsSymbol = !resolvedNames.isEmpty();
                    return targetNameMatches || exportsSymbol;
                  });
      if (validTarget.isDefined()) {
        importDefs.add(impIR);
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

  private List<Import.Module> findImportIRs(ResolvedName resolvedName) {
    var modName = resolvedName.module().getName();
    var symName = resolvedName.qualifiedName();
    return findImportIRs(modName, symName);
  }

  private void addUsedSymbolForResolution(Resolution resolution) {
    if (resolution != null) {
      var modName = resolution.target().module().getName();
      var symName = resolution.target().qualifiedName();
      addUsedSymbol(modName, symName);
    }
  }

  private void addUsedSymbol(QualifiedName modName, QualifiedName symName) {
    var imports = findImportIRs(modName, symName);
    for (var imp : imports) {
      var symNameToRecord = symName;
      // Add only prefix in some cases
      if (imp.onlyNames().isDefined()) {
        var onlyNames = imp.onlyNames().get();
        var baseName = QualifiedName.fromString(imp.name().name());
        for (var onlyName : asJava(onlyNames)) {
          var importFqn = baseName.createChild(onlyName.name());
          if (isPrefix(importFqn, symName)) {
            symNameToRecord = importFqn;
          }
        }
      }
      LOGGER.trace(
          "[{}] Adding used symbol '{}' for import '{}'",
          bindingsMap.currentModule().getName(),
          symNameToRecord,
          imp.showCode());
      usedSymbolsBldr.addUsedSymbol(imp, symNameToRecord);
    }
  }

  private static boolean isPrefix(QualifiedName prefix, QualifiedName name) {
    var namePath = name.fullPath();
    var subNamePath = prefix.fullPath();
    if (namePath.size() < subNamePath.size()) {
      return false;
    }
    for (var i = 0; i < subNamePath.size(); i++) {
      var subNameItem = subNamePath.apply(i);
      var nameItem = namePath.apply(i);
      if (!nameItem.equals(subNameItem)) {
        return false;
      }
    }
    return true;
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

  private static TypeSignatures.Signature getTypeSignatureMeta(IR ir) {
    return MetadataInteropHelpers.getMetadataOrNull(
        ir, TypeSignatures$.MODULE$, TypeSignatures.Signature.class);
  }

  private static BindingsMap.Resolution getTypeNameMeta(IR ir) {
    return MetadataInteropHelpers.getMetadataOrNull(
        ir, TypeNames$.MODULE$, BindingsMap.Resolution.class);
  }

  private static BindingsMap.Resolution getMethodDefinitionsMeta(IR ir) {
    return MetadataInteropHelpers.getMetadataOrNull(
        ir, MethodDefinitions.INSTANCE, BindingsMap.Resolution.class);
  }

  private static BindingsMap.Resolution getGlobalNamesMeta(IR ir) {
    return MetadataInteropHelpers.getMetadataOrNull(
        ir, GlobalNames$.MODULE$, BindingsMap.Resolution.class);
  }

  private static BindingsMap.Resolution getPatternsMeta(IR ir) {
    return MetadataInteropHelpers.getMetadataOrNull(
        ir, Patterns$.MODULE$, BindingsMap.Resolution.class);
  }

  private static Annotations getGenericAnnotationMeta(IR ir) {
    return MetadataInteropHelpers.getMetadataOrNull(
        ir, GenericAnnotations$.MODULE$, ModuleAnnotations.Annotations.class);
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

  private static String importDefsToString(List<Import.Module> imps) {
    var str =
        imps.stream().map(imp -> "'" + imp.showCode() + "'").collect(Collectors.joining(", "));
    return "[" + str + "]";
  }

  private static String irToStr(IR ir) {
    return ir.getClass().getName() + "@" + identityHex(ir);
  }

  private static String identityHex(Object obj) {
    return Integer.toHexString(Objects.hashCode(obj));
  }
}
