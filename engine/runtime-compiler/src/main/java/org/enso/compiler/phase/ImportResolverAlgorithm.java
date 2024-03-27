package org.enso.compiler.phase;


import java.util.Objects;

import org.enso.compiler.Compiler;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap$ModuleReference$Concrete;
import org.enso.compiler.data.BindingsMap.ResolvedImport;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.editions.LibraryName;
import org.enso.polyglot.CompilationStage;

import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.jdk.CollectionConverters;

public abstract class ImportResolverAlgorithm {
  protected ImportResolverAlgorithm() {
  }

  abstract Compiler getCompiler();
  abstract Name.Qualified nameForImport(Import.Module imp);
  abstract Name.Qualified nameForExport(Export.Module ex);
  abstract String nameForType(ResolvedType e);
  abstract java.util.List<Export.Module> exportsFor(Module module, String impName);
  abstract boolean isAll(Export.Module ex); /* ex.isAll */
  /** @return {@code null} or list of named imports */
  abstract java.util.List<Name.Literal> onlyNames(Export.Module ex);
  /** @return {@code null} or list of named imports */
  abstract java.util.List<Name.Literal> hiddenNames(Export.Module ex);
  abstract java.util.List<ResolvedType> definedEntities(String name);
  abstract Tuple2<Import, Option<ResolvedImport>> tupleResolvedImport(Import.Module imp, java.util.List<Export.Module> exp, CompilerContext.Module m);
  abstract Tuple2<Import, Option<ResolvedImport>> tupleResolvedType(Import.Module imp, java.util.List<Export.Module> exp, ResolvedType m);
  abstract Tuple2<Import, Option<ResolvedImport>> tupleErrorPackageCoundNotBeLoaded(Import.Module imp, String impName, String loadingError);
  abstract Tuple2<Import, Option<ResolvedImport>> tupleErrorModuleDoesNotExist(Import.Module imp, String impName);

  public Tuple2<Import, Option<ResolvedImport>> tryResolveImport(
    Module module,
    Import.Module imp
  ) {
    var res = tryResolveImportNew(module, imp);
    return res;
  }

  private Tuple2<Import, Option<ResolvedImport>> tryResolveImportNew(
    Module module,
    Import.Module imp
  ) {
    var impName = nameForImport(imp).name();
    var exp = exportsFor(module, impName);
    var fromAllExports = exp.stream().filter(ex -> isAll(ex)).toList();
    if (fromAllExports.size() >= 2) {
        // Detect potential conflicts when importing all and hiding names for the exports of the same module
        var unqualifiedImports = fromAllExports.stream().filter(e -> onlyNames(e) == null).toList();
        var qualifiedImports = fromAllExports.stream().map(e -> {
            var onlyNames = onlyNames(e);
            if (onlyNames != null) {
                return onlyNames.stream().map(n -> n.name()).toList();
            } else {
                return null;
            }
        }).filter(Objects::nonNull).toList();
        var importsWithHiddenNames = fromAllExports.stream().map(e -> {
            var hiddenNames = hiddenNames(e);
            if (hiddenNames != null) {
                return new Tuple2<>(e, hiddenNames);
            } else {
                return null;
            }
        }).filter(Objects::nonNull).toList();

        for (var h : importsWithHiddenNames) {
          var e = h._1;
          var hidden = h._2;
          var unqualifiedConflicts = unqualifiedImports.stream().filter(x -> !x.equals(e)).filter(Objects::nonNull).toList();
          if (!unqualifiedConflicts.isEmpty()) {
            var b = hidden.stream().map(x -> x.name()).toList();
            throw HiddenNamesConflict.shadowUnqualifiedExport(
              nameForExport(e).name(), b
            );
          }
        }
        for (var h : importsWithHiddenNames) {
          var e = h._1;
          var hidden = h._2;
          var qualifiedConflicts = qualifiedImports.stream().filter(Objects::nonNull)
            .flatMap(x -> x.stream())
            .filter(f -> hidden.stream().filter(x -> f.equals(x.name())).findAny().isPresent())
            .toList();
          if (!qualifiedConflicts.isEmpty()) {
            throw HiddenNamesConflict.shadowQualifiedExport(
              nameForExport(e).name(), qualifiedConflicts
            );
          }
        }
    }
    var parts = nameForImport(imp).parts();
    if (parts.length() < 2) {
        throw new CompilerError(
          "Imports should contain at least two segments after " +
          "desugaring."
        );
    }
    var compiler = this.getCompiler();
    var repo = compiler.packageRepository();
    var twoParts = parts.take(2);
    var libraryName = new LibraryName(twoParts.head().name(), twoParts.last().name());
    var foundLib = repo.ensurePackageIsLoaded(libraryName);
    if (foundLib.isRight()) {
        var moduleOption = compiler.getModule(impName);
        if (moduleOption.isDefined()) {
            var m = moduleOption.get();
            return tupleResolvedImport(imp, exp, m);
        } else {
            var typ = tryResolveAsTypeNew(nameForImport(imp));
            if (typ != null) {
                return tupleResolvedType(imp, exp, typ);
            } else {
                return tupleErrorModuleDoesNotExist(imp, impName);
            }
        }
    } else {
        var loadingError = foundLib.left().getOrElse(null).toString();
        return tupleErrorPackageCoundNotBeLoaded(imp, impName, loadingError);
    }
  }

  private ResolvedType tryResolveAsTypeNew(Name.Qualified name) {
    var parts = CollectionConverters.SeqHasAsJava(name.parts()).asJava();
    var last = parts.size() - 1;
    var tp  = parts.get(last).name();
    var modName = String.join(".", parts.subList(0, last).stream().map(n -> n.name()).toList());
    var entities = definedEntities(modName);
    if (entities == null) {
      return null;
    }
    var type = definedEntities(modName).stream()
      .filter(e -> nameForType(e).equals(tp))
      .findFirst();
    return type.orElse(null);
  }

  private static <T> List<T> toScalaList(java.util.List<T> qualifiedConflicts) {
    return CollectionConverters.ListHasAsScala(qualifiedConflicts).asScala().toList();
  }

  public static final class HiddenNamesConflict extends RuntimeException {
    private HiddenNamesConflict(String message) {
      super(message);
    }

    static HiddenNamesConflict shadowUnqualifiedExport(String name, java.util.List<String> hiddenNames) {
      String msg;
      if (hiddenNames.size() == 1) {
        msg = "Hidden '${conflict}' name of the export module ${name} conflicts with the unqualified export"
          .replace("${name}", name)
          .replace("${conflict}", hiddenNames.get(0));
      } else {
        msg = "Hidden '${conflict}' names of the export module ${name} conflict with the unqualified export"
          .replace("${name}", name)
          .replace("${conflict}", String.join(",", hiddenNames));
      }
      return new HiddenNamesConflict(msg);
    }

    static HiddenNamesConflict shadowQualifiedExport(String name, java.util.List<String> conflict) {
      String msg;
      if (conflict.size() == 1) {
        msg = "Hidden '${conflict}' name of the exported module ${name} conflicts with the qualified export"
          .replace("${name}", name)
          .replace("${conflict}", conflict.get(0));
      } else {
        msg = "Hidden '${conflict}' names of the exported module ${name} conflicts with the qualified export"
          .replace("${name}", name)
          .replace("${conflict}", String.join(",", conflict));
      }
      return new HiddenNamesConflict(msg);
    }
  }

  abstract static class Impl extends ImportResolverAlgorithm {
    @Override
    Name.Qualified nameForImport(Import.Module imp) {
      return imp.name();
    }

    @Override
    Name.Qualified nameForExport(Export.Module ex) {
      return ex.name();
    }

    @Override
    String nameForType(ResolvedType e) {
      return e.qualifiedName().item();
    }

    @Override
    java.util.List<Export.Module> exportsFor(Module module, String impName) {
      var exp = CollectionConverters.SeqHasAsJava(module.exports()).asJava().stream().map(e -> switch (e) {
        case Export.Module ex when ex.name().name().equals(impName) -> ex;
        case null, default -> null;
      }).filter(Objects::nonNull).toList();
      return exp;
    }

    @Override
    boolean isAll(Export.Module ex) {
      return ex.isAll();
    }

    @Override
    java.util.List<Name.Literal> onlyNames(Export.Module ex) {
      if (ex.onlyNames().isEmpty()) {
        return null;
      }
      var list = CollectionConverters.SeqHasAsJava(ex.onlyNames().get()).asJava();
      return list;
    }

    @Override
    java.util.List<Name.Literal> hiddenNames(Export.Module ex) {
      if (ex.hiddenNames().isEmpty()) {
        return null;
      }
      var list = CollectionConverters.SeqHasAsJava(ex.hiddenNames().get()).asJava();
      return list;
    }

    @Override
    java.util.List<ResolvedType> definedEntities(String name) {
      var compiler = this.getCompiler();
      var optionMod = compiler.getModule(name);
      if (optionMod.isEmpty()) {
        return null;
      }

      var mod= optionMod.get();
      compiler.ensureParsed(mod);
      var b = mod.getBindingsMap();
      if (b == null) {
        compiler.context().updateModule(mod, u -> {
          u.invalidateCache();
          u.ir(null);
          u.compilationStage(CompilationStage.INITIAL);
        });
        compiler.ensureParsed(mod, false);
        b = mod.getBindingsMap();
      }
      List<ResolvedType> entitiesStream = b.definedEntities()
        .map(e -> switch (e) {
          case BindingsMap.Type t -> {
            assert e.name().equals(t.name()) : e.name() + " != " + t.name();
            var res = new ResolvedType(new BindingsMap$ModuleReference$Concrete(mod), t);
            assert e.name().equals(res.tp().name()) : e.name() + " != " + res.tp().name();
            yield res;
          }
          case null, default -> null;
        })
        .filter(Objects::nonNull);
      var entities = CollectionConverters.SeqHasAsJava(entitiesStream).asJava();
      return entities;
    }

    @Override
    Tuple2<Import, Option<ResolvedImport>> tupleResolvedImport(Import.Module imp, java.util.List<Export.Module> exp, CompilerContext.Module m) {
      var someBinding = Option.apply(new BindingsMap.ResolvedImport(
        imp,
        toScalaList(exp),
        new BindingsMap.ResolvedModule(new BindingsMap$ModuleReference$Concrete(m))
      ));
      return new Tuple2<>(imp, someBinding);
    }

    @Override
    Tuple2<Import, Option<ResolvedImport>> tupleResolvedType(Import.Module imp, java.util.List<Export.Module> exp, ResolvedType typ) {
      var someBinding = Option.apply(new BindingsMap.ResolvedImport(imp, toScalaList(exp), typ));
      return new Tuple2<>(imp, someBinding);
    }

    @Override
    Tuple2<Import, Option<ResolvedImport>> tupleErrorPackageCoundNotBeLoaded(Import.Module imp, String impName, String loadingError) {
      var importError = new ImportExport(
        imp,
        new ImportExport.PackageCouldNotBeLoaded(
          impName,
          loadingError
        ), imp.passData(), imp.diagnostics()
      );
      return new Tuple2<>(
        importError, Option.empty()
      );
    }

    @Override
    Tuple2<Import, Option<ResolvedImport>> tupleErrorModuleDoesNotExist(Import.Module imp, String impName) {
      return new Tuple2<>(
        new ImportExport(
          imp,
          new ImportExport.ModuleDoesNotExist(impName),
            imp.passData(), imp.diagnostics()
        ),
        Option.empty()
      );
    }

  }
}
