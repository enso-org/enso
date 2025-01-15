package org.enso.compiler.phase;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.compiler.core.CompilerError;
import org.enso.editions.LibraryName;

public abstract class ImportResolverAlgorithm<
    Result,
    Module,
    Import,
    Export,
    ResolvedType,
    ResolvedModule,
    ResolvedConstructor,
    ResolvedModuleMethod,
    ResolvedExtensionMethod,
    ResolvedConversionMethod> {
  protected ImportResolverAlgorithm() {}

  protected abstract String nameForImport(Import imp);

  protected abstract List<String> partsForImport(Import imp);

  protected abstract String nameForExport(Export ex);

  protected abstract String nameForType(ResolvedType e);

  protected abstract String nameForConstructor(ResolvedConstructor cons);

  protected abstract String nameForModuleMethod(ResolvedModuleMethod method);

  protected abstract String nameForExtensionMethod(ResolvedExtensionMethod method);

  protected abstract String nameForConversionMethod(ResolvedConversionMethod method);

  /**
   * Returns a list of all the exports from the module of the given symbol
   *
   * @param module the module to search for exports in
   * @param symbol FQN symbol contained in the returned exports.
   */
  protected abstract java.util.List<Export> exportsFor(Module module, String symbol);

  /**
   * @return {@code null} or list of named imports
   */
  protected abstract java.util.List<String> onlyNames(Export ex);

  protected abstract java.util.List<ResolvedType> definedEntities(Import name);

  protected abstract java.util.List<ResolvedConstructor> definedConstructors(Import name);

  protected abstract java.util.List<ResolvedModuleMethod> definedModuleMethods(Import imp);

  protected abstract java.util.List<ResolvedExtensionMethod> definedExtensionMethods(Import imp);

  protected abstract java.util.List<ResolvedConversionMethod> definedConversionMethods(Import imp);

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

  protected abstract Result createResolvedConstructor(
      Import imp, java.util.List<Export> exp, ResolvedConstructor cons);

  protected abstract Result createResolvedModuleMethod(
      Import imp, java.util.List<Export> exp, ResolvedModuleMethod moduleMethod);

  protected abstract Result createResolvedExtensionMethods(
      Import imp,
      java.util.List<Export> exp,
      java.util.List<ResolvedExtensionMethod> extensionMethods);

  protected abstract Result createResolvedConversionMethods(
      Import imp,
      java.util.List<Export> exp,
      java.util.List<ResolvedConversionMethod> conversionMethods);

  protected abstract Result createErrorPackageCoundNotBeLoaded(
      Import imp, String impName, String loadingError);

  protected abstract Result createErrorModuleDoesNotExist(Import imp, String impName);

  /**
   * Resolves given {@code Import} in context of given {@code Module}. Handles only an import of a
   * specific symbol with syntax {@code import project.Module.Symbol}. The {@code from
   * project.Module import Symbol} is handled by this algorithm by importing only the {@code Module}
   * and not resolving the {@code Symbol}.
   *
   * <p>With the first syntax of import ({@code import project.Module.Symbol}), one case only import
   * module, type from a module, constructor from a type or a module method. Static, extension and
   * conversion methods can be imported only with the {@code from ... import all} syntax.
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
      }
      var typ = tryResolveAsTypeNew(imp);
      if (typ != null) {
        return createResolvedType(imp, exp, typ);
      }
      var cons = tryResolveAsConstructor(imp);
      if (cons != null) {
        return createResolvedConstructor(imp, exp, cons);
      }
      var moduleMethod = tryResolveAsModuleMethod(imp);
      if (moduleMethod != null) {
        return createResolvedModuleMethod(imp, exp, moduleMethod);
      }
      var extensionMethods = tryResolveAsExtensionMethods(imp);
      if (extensionMethods != null) {
        return createResolvedExtensionMethods(imp, exp, extensionMethods);
      }
      var conversionMethods = tryResolveAsConversionMethods(imp);
      if (conversionMethods != null) {
        return createResolvedConversionMethods(imp, exp, conversionMethods);
      }
      return createErrorModuleDoesNotExist(imp, impName);
    } catch (IOException e) {
      return createErrorPackageCoundNotBeLoaded(imp, impName, e.getMessage());
    }
  }

  private ResolvedType tryResolveAsTypeNew(Import name) {
    var parts = partsForImport(name);
    var last = parts.size() - 1;
    var tp = parts.get(last);
    var entities = definedEntities(name);
    if (entities == null) {
      return null;
    }
    var type = entities.stream().filter(e -> nameForType(e).equals(tp)).findFirst();
    return type.orElse(null);
  }

  private ResolvedConstructor tryResolveAsConstructor(Import imp) {
    var parts = partsForImport(imp);
    if (parts.size() < 3) {
      return null;
    }
    var constructors = definedConstructors(imp);
    if (constructors == null) {
      return null;
    }
    var constrName = parts.get(parts.size() - 1);
    var consOpt =
        constructors.stream()
            .filter(cons -> nameForConstructor(cons).equals(constrName))
            .findFirst();
    return consOpt.orElse(null);
  }

  private ResolvedModuleMethod tryResolveAsModuleMethod(Import imp) {
    var parts = partsForImport(imp);
    if (parts.size() < 3) {
      return null;
    }
    var moduleMethods = definedModuleMethods(imp);
    if (moduleMethods == null) {
      return null;
    }
    var methodName = parts.get(parts.size() - 1);
    var methodOpt =
        moduleMethods.stream()
            .filter(method -> nameForModuleMethod(method).equals(methodName))
            .findFirst();
    return methodOpt.orElse(null);
  }

  /**
   * Tries to resolve the given import as a list of extension methods. Note that it is possible that
   * a single symbol resolves to multiple extension methods.
   *
   * @return List with at least one element. null if there are no static methods in the imported
   *     module scope.
   */
  private java.util.List<ResolvedExtensionMethod> tryResolveAsExtensionMethods(Import imp) {
    var parts = partsForImport(imp);
    if (parts.size() < 3) {
      return null;
    }
    var definedExtensionMethods = definedExtensionMethods(imp);
    if (definedExtensionMethods == null) {
      return null;
    }
    var methodName = parts.get(parts.size() - 1);
    var foundExtMethods =
        definedExtensionMethods.stream()
            .filter(method -> nameForExtensionMethod(method).equals(methodName))
            .collect(Collectors.toUnmodifiableList());
    if (foundExtMethods.isEmpty()) {
      return null;
    } else {
      return foundExtMethods;
    }
  }

  /**
   * Tries to resolve the given import as a list of conversion methods. Note that it is possible
   * that a single symbol resolves to multiple extension methods.
   *
   * @return List of at least one element. null if there are no conversion methods in the imported
   *     module scope.
   */
  private java.util.List<ResolvedConversionMethod> tryResolveAsConversionMethods(Import imp) {
    var parts = partsForImport(imp);
    if (parts.size() < 3) {
      return null;
    }
    var definedConvMethods = definedConversionMethods(imp);
    if (definedConvMethods == null) {
      return null;
    }
    var methodName = parts.get(parts.size() - 1);
    var foundConvMethods =
        definedConvMethods.stream()
            .filter(method -> nameForConversionMethod(method).equals(methodName))
            .collect(Collectors.toUnmodifiableList());
    if (foundConvMethods.isEmpty()) {
      return null;
    } else {
      return foundConvMethods;
    }
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
