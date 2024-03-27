package org.enso.compiler.phase;

import java.io.IOException;
import java.util.List;
import java.util.Objects;
import org.enso.compiler.core.CompilerError;
import org.enso.editions.LibraryName;
import scala.Tuple2;

public abstract class ImportResolverAlgorithm<
    Result, Module, Import, Export, ResolvedType, ResolvedModule> {
  protected ImportResolverAlgorithm() {}

  protected abstract String nameForImport(Import imp);

  protected abstract List<String> partsForImport(Import imp);

  protected abstract String nameForExport(Export ex);

  protected abstract String nameForType(ResolvedType e);

  protected abstract java.util.List<Export> exportsFor(Module module, String impName);

  protected abstract boolean isAll(Export ex);

  /**
   * @return {@code null} or list of named imports
   */
  protected abstract java.util.List<String> onlyNames(Export ex);

  /**
   * @return {@code null} or list of named imports
   */
  protected abstract java.util.List<String> hiddenNames(Export ex);

  protected abstract java.util.List<ResolvedType> definedEntities(Import name);

  /**
   * Ensure library is loaded and load a module.
   *
   * @return {@code null} if the library is loaded, but the module isn't found
   * @throws IOException with {@link IOException#getMessage()} when the library cannot be loaded
   */
  protected abstract ResolvedModule loadLibraryModule(LibraryName libraryName, String moduleName)
      throws IOException;

  protected abstract Result createResolvedImport(
      Import imp, java.util.List<Export> exp, ResolvedModule m);

  protected abstract Result createResolvedType(
      Import imp, java.util.List<Export> exp, ResolvedType m);

  protected abstract Result createErrorPackageCoundNotBeLoaded(
      Import imp, String impName, String loadingError);

  protected abstract Result createErrorModuleDoesNotExist(Import imp, String impName);

  /**
   * Resolves given {@code Import} in context of given {@code Module}.
   *
   * @param module the module that wants to import something
   * @param imp the import to resolve
   * @return {@code Result} as created by {@link #createResolvedImport} and similar methods
   */
  public final Result tryResolveImport(Module module, Import imp) {
    var res = tryResolveImportNew(module, imp);
    return res;
  }

  private Result tryResolveImportNew(Module module, Import imp) {
    var impName = nameForImport(imp);
    var exp = exportsFor(module, impName);
    var fromAllExports = exp.stream().filter(ex -> isAll(ex)).toList();
    if (fromAllExports.size() >= 2) {
      // Detect potential conflicts when importing all and hiding names for the exports of the same
      // module
      var unqualifiedImports = fromAllExports.stream().filter(e -> onlyNames(e) == null).toList();
      var qualifiedImports =
          fromAllExports.stream()
              .map(
                  e -> {
                    var onlyNames = onlyNames(e);
                    if (onlyNames != null) {
                      return onlyNames.stream().toList();
                    } else {
                      return null;
                    }
                  })
              .filter(Objects::nonNull)
              .toList();
      var importsWithHiddenNames =
          fromAllExports.stream()
              .map(
                  e -> {
                    var hiddenNames = hiddenNames(e);
                    if (hiddenNames != null) {
                      return new Tuple2<>(e, hiddenNames);
                    } else {
                      return null;
                    }
                  })
              .filter(Objects::nonNull)
              .toList();

      for (var h : importsWithHiddenNames) {
        var e = h._1;
        var hidden = h._2;
        var unqualifiedConflicts =
            unqualifiedImports.stream().filter(x -> !x.equals(e)).filter(Objects::nonNull).toList();
        if (!unqualifiedConflicts.isEmpty()) {
          throw HiddenNamesConflict.shadowUnqualifiedExport(nameForExport(e), hidden);
        }
      }
      for (var h : importsWithHiddenNames) {
        var e = h._1;
        var hidden = h._2;
        var qualifiedConflicts =
            qualifiedImports.stream()
                .filter(Objects::nonNull)
                .flatMap(x -> x.stream())
                .filter(f -> hidden.stream().filter(x -> f.equals(x)).findAny().isPresent())
                .toList();
        if (!qualifiedConflicts.isEmpty()) {
          throw HiddenNamesConflict.shadowQualifiedExport(nameForExport(e), qualifiedConflicts);
        }
      }
    }
    var parts = partsForImport(imp);
    if (parts.size() < 2) {
      throw new CompilerError(
          "Imports should contain at least two segments after " + "desugaring.");
    }
    var libraryName = new LibraryName(parts.get(0), parts.get(1));
    try {
      var m = loadLibraryModule(libraryName, impName);
      if (m != null) {
        return createResolvedImport(imp, exp, m);
      } else {
        var typ = tryResolveAsTypeNew(imp);
        if (typ != null) {
          return createResolvedType(imp, exp, typ);
        } else {
          return createErrorModuleDoesNotExist(imp, impName);
        }
      }
    } catch (IOException e) {
      return createErrorPackageCoundNotBeLoaded(imp, impName, e.getMessage());
    }
  }

  private ResolvedType tryResolveAsTypeNew(Import name) {
    var parts = partsForImport(name);
    var last = parts.size() - 1;
    var tp = parts.get(last);
    var modName = String.join(".", parts.subList(0, last).stream().toList());
    var entities = definedEntities(name);
    if (entities == null) {
      return null;
    }
    var type = entities.stream().filter(e -> nameForType(e).equals(tp)).findFirst();
    return type.orElse(null);
  }

  public static final class HiddenNamesConflict extends RuntimeException {
    private HiddenNamesConflict(String message) {
      super(message);
    }

    static HiddenNamesConflict shadowUnqualifiedExport(
        String name, java.util.List<String> hiddenNames) {
      String msg;
      if (hiddenNames.size() == 1) {
        msg =
            "Hidden '${conflict}' name of the export module ${name} conflicts with the unqualified export"
                .replace("${name}", name)
                .replace("${conflict}", hiddenNames.get(0));
      } else {
        msg =
            "Hidden '${conflict}' names of the export module ${name} conflict with the unqualified export"
                .replace("${name}", name)
                .replace("${conflict}", String.join(",", hiddenNames));
      }
      return new HiddenNamesConflict(msg);
    }

    static HiddenNamesConflict shadowQualifiedExport(String name, java.util.List<String> conflict) {
      String msg;
      if (conflict.size() == 1) {
        msg =
            "Hidden '${conflict}' name of the exported module ${name} conflicts with the qualified export"
                .replace("${name}", name)
                .replace("${conflict}", conflict.get(0));
      } else {
        msg =
            "Hidden '${conflict}' names of the exported module ${name} conflicts with the qualified export"
                .replace("${name}", name)
                .replace("${conflict}", String.join(",", conflict));
      }
      return new HiddenNamesConflict(msg);
    }
  }
}
