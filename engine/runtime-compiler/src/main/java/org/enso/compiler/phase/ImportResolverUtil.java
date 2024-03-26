package org.enso.compiler.phase;


import org.enso.compiler.core.ir.expression.errors.ImportExport;

import org.enso.compiler.Compiler;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.data.BindingsMap$ModuleReference$Concrete;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.editions.LibraryName;

import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.jdk.CollectionConverters;

public final class ImportResolverUtil extends ImportResolver {
  public ImportResolverUtil(Compiler c) {
    super(c);
  }

  @Override
  public Tuple2<Import, Option<BindingsMap.ResolvedImport>> tryResolveImport(
    Module module,
    Import.Module imp
  ) {
      var res = tryResolveImportNew(module, imp);

      var old = res;
      assert res.equals(old = tryResolveImportOld(module, imp)) : "old:\n" + old._1.pretty() + "\nnew:\n" + res._1.pretty();

      return res;
  }

  private Tuple2<Import, Option<BindingsMap.ResolvedImport>> tryResolveImportNew(
    Module module,
    Import.Module imp
  ) {
    var impName = imp.name().name();
    var exp = CollectionConverters.SeqHasAsJava(module.exports()).asJava().stream().map(e -> switch (e) {
        case Export.Module ex when ex.name().name().equals(impName) -> ex;
        case null, default -> null;
    }).filter(e -> e != null).toList();
    var fromAllExports = exp.stream().filter(ex -> ex.isAll()).toList();

    // switch (fromAllExports) {
    //   case _ :: _ :: _ =>

    if (fromAllExports.size() >= 2) {
        // Detect potential conflicts when importing all and hiding names for the exports of the same module
        var unqualifiedImports = fromAllExports.stream().filter(e -> e.onlyNames().isEmpty()).toList();
        var qualifiedImports = fromAllExports.stream().map(e -> {
            if (e.onlyNames().isDefined()) {
                var onlyNames = CollectionConverters.SeqHasAsJava(e.onlyNames().get()).asJava();
                return onlyNames.stream().map(n -> n.name()).toList();
            } else {
                return null;
            }
        }).filter(e -> e != null).toList();
        var importsWithHiddenNames = fromAllExports.stream().map(e -> {
            if (e.hiddenNames().isDefined()) {
                var hiddenNames = CollectionConverters.SeqHasAsJava(e.hiddenNames().get()).asJava();
                return new Tuple2<>(e, hiddenNames);
            } else {
                return null;
            }
        }).filter(e -> e != null).toList();

        for (var h : importsWithHiddenNames) {
            var e = h._1;
            var hidden = h._2;
            var unqualifiedConflicts = unqualifiedImports.stream().filter(x -> x != e).toList();
            if (!unqualifiedConflicts.isEmpty()) {
              var b = toScalaList(hidden.stream().map(x -> x.name()).toList());
              throw new HiddenNamesShadowUnqualifiedExport(
                e.name().name(), b
              );
            }

        }
        for (var h : importsWithHiddenNames) {
            var e = h._1;
            var hidden = h._2;
            var qualifiedConflicts = qualifiedImports.stream().filter(x -> x != e)
              .flatMap(x -> x.stream())
              .filter(f -> hidden.stream().filter(x -> f.equals(x.name())).findAny().isPresent())
              .toList();
          if (!qualifiedConflicts.isEmpty()) {
              var b = toScalaList(qualifiedConflicts);
            throw new HiddenNamesShadowQualifiedExport(
              e.name().name(), b

            );
          }
        }
    };
    var parts = imp.name().parts();
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
            var someBinding = Option.apply(new BindingsMap.ResolvedImport(
                  imp,
                  toScalaList(exp),
                  new ResolvedModule(new BindingsMap$ModuleReference$Concrete(m))
            ));
            return new Tuple2<>(imp, someBinding);
        } else {
            var optionTp = tryResolveAsType(imp.name());
            if (optionTp.isDefined()) {
                var someBinding = Option.apply(new BindingsMap.ResolvedImport(imp, toScalaList(exp), optionTp.get()));
                return new Tuple2<>(imp, someBinding);
            } else {
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
    } else {
        var loadingError = foundLib.left().getOrElse(String::new).toString();
        var importError = new ImportExport(
            imp,
            new ImportExport.PackageCouldNotBeLoaded(
              impName,
              loadingError
            ), imp.passData(), imp.diagnostics());
        return new Tuple2<>(
            importError, Option.empty()
        );
    }
  }

  private static <T> List<T> toScalaList(java.util.List<T> qualifiedConflicts) {
    return CollectionConverters.ListHasAsScala(qualifiedConflicts).asScala().toList();
  }

}
