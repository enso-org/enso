package org.enso.interpreter.runtime.scope;

import java.io.File;
import java.util.Collection;
import java.util.Optional;
import java.util.concurrent.ExecutionException;

import org.enso.common.MethodNames;
import org.enso.compiler.PackageRepository;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.type.Types;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.pkg.NativeLibraryFinder;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.scala.wrapper.ScalaConversions;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;

import scala.Option;

/** Represents the top scope of Enso execution, containing all the importable modules. */
@ExportLibrary(InteropLibrary.class)
public final class TopLevelScope extends EnsoObject {
  private final Builtins builtins;
  private final PackageRepository packageRepository;

  /**
   * Creates a new instance of top scope.
   *
   * @param builtins the automatically-imported builtin module.
   * @param packageRepository the {@link PackageRepository} instance that manages loaded packages
   */
  public TopLevelScope(Builtins builtins, PackageRepository packageRepository) {
    this.builtins = builtins;
    this.packageRepository = packageRepository;
  }

  /**
   * @return the list of modules in the scope.
   */
  @SuppressWarnings("unchecked")
  public Collection<Module> getModules() {
    var filtered = packageRepository.getLoadedModules().map(Module::fromCompilerModule);
    return ScalaConversions.asJava(filtered.toSeq());
  }

  /**
   * Looks up a module by name.
   *
   * @param name the name of the module to look up.
   * @return empty result if the module does not exist or the requested module.
   */
  public Optional<Module> getModule(String name) {
    return ScalaConversions.asJava(
        packageRepository.getLoadedModule(name).map(Module::fromCompilerModule));
  }

  /**
   * Creates and registers a new module with given name and source file.
   *
   * @param name the module name.
   * @param sourceFile the module source file.
   * @return the newly created module.
   */
  public Module createModule(QualifiedName name, Package<TruffleFile> pkg, TruffleFile sourceFile) {
    Module module = new Module(name, pkg, sourceFile);
    packageRepository.registerModuleCreatedInRuntime(module.asCompilerModule());
    return module;
  }

  public Module createModule(QualifiedName name, Package<TruffleFile> pkg, String source) {
    Module module = new Module(name, pkg, source);
    packageRepository.registerModuleCreatedInRuntime(module.asCompilerModule());
    return module;
  }

  /**
   * Returns the builtins module.
   *
   * @return the builtins module.
   */
  public Builtins getBuiltins() {
    return builtins;
  }

  /**
   * Marks this object as having members accessible through the polyglot API.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /**
   * Returns a collection of all the members of this scope.
   *
   * <p>The exported members are the {@code get_module} and {@code create_module} methods.
   *
   * @param includeInternal ignored.
   * @return a collection of all the exported members.
   */
  @ExportMessage
  EnsoObject getMembers(boolean includeInternal) {
    return ArrayLikeHelpers.wrapStrings(
        MethodNames.TopScope.GET_MODULE,
        MethodNames.TopScope.CREATE_MODULE,
        MethodNames.TopScope.REGISTER_MODULE,
        MethodNames.TopScope.UNREGISTER_MODULE,
        MethodNames.TopScope.COMPILE);
  }

  /** Handles member invocation through the polyglot API. */
  @ExportMessage
  abstract static class InvokeMember {
    @CompilerDirectives.TruffleBoundary
    private static Module getModule(TopLevelScope scope, Object[] arguments)
        throws ArityException, UnsupportedTypeException, UnknownIdentifierException {
      String moduleName = Types.extractArguments(arguments, String.class);

      var module = scope.getModule(moduleName);
      if (module.isEmpty()) {
        throw new PanicException(
            scope.builtins.error().makeModuleDoesNotExistError(moduleName), null);
      }

      return module.get();
    }

    @CompilerDirectives.TruffleBoundary
    private static Module createModule(TopLevelScope scope, Object[] arguments, EnsoContext context)
        throws ArityException, UnsupportedTypeException {
      String moduleName = Types.extractArguments(arguments, String.class);
      return Module.empty(QualifiedName.simpleName(moduleName), null);
    }

    @CompilerDirectives.TruffleBoundary
    private static Module registerModule(
        TopLevelScope scope, Object[] arguments, EnsoContext context)
        throws ArityException, UnsupportedTypeException {
      Types.Pair<String, String> args =
          Types.extractArguments(arguments, String.class, String.class);
      QualifiedName qualName = QualifiedName.fromString(args.getFirst());
      File location = new File(args.getSecond());
      var libName = LibraryName.fromModuleName(qualName.toString());
      Package<TruffleFile> pkg = null;
      if (libName.isDefined()) {
        var pkgOpt = context.getPackageRepository().getPackageForLibraryJava(libName.get());
        if (pkgOpt.isPresent()) {
          pkg = pkgOpt.get();
        }
      }
      Module module = new Module(qualName, pkg, context.getTruffleFile(location));
      scope.packageRepository.registerModuleCreatedInRuntime(module.asCompilerModule());
      return module;
    }

    @CompilerDirectives.TruffleBoundary
    private static Object findNativeLibrary(Object[] arguments, EnsoContext context) {
      var libname = arguments[0].toString();
      var pkgRepo = context.getPackageRepository();
      for (var pkg : pkgRepo.getLoadedPackagesJava()) {
        var libPath = NativeLibraryFinder.findNativeLibrary(libname, pkg, TruffleFileSystem.INSTANCE);
        if (libPath != null) {
          return libPath;
        }
      }
      return context.getNothing();
    }

    @CompilerDirectives.TruffleBoundary
    private static Object unregisterModule(
        TopLevelScope scope, Object[] arguments, EnsoContext context)
        throws ArityException, UnsupportedTypeException {
      String name = Types.extractArguments(arguments, String.class);
      scope.packageRepository.deregisterModule(name);
      return context.getNothing();
    }

    private static Object leakContext(EnsoContext context) {
      return context.asGuestValue(context);
    }

    @CompilerDirectives.TruffleBoundary
    private static Object compile(Object[] arguments, EnsoContext context)
        throws UnsupportedTypeException, ArityException {
      boolean useGlobalCache = context.isUseGlobalCache();
      boolean shouldCompileDependencies;
      scala.Option<String> generateDocs;
      switch (arguments.length) {
        case 2 -> {
          shouldCompileDependencies = Boolean.TRUE.equals(arguments[0]);
          generateDocs = switch (arguments[1]) {
            case Boolean b when !b -> Option.empty();
            case String s -> Option.apply(s);
            default -> Option.empty();
          };
        }
        default -> {
          shouldCompileDependencies = Types.extractArguments(arguments, Boolean.class);
          generateDocs = Option.empty();
        }
      }
      boolean shouldWriteCache = !context.isIrCachingDisabled();
      try {
        return context
            .getCompiler()
            .compile(shouldCompileDependencies, shouldWriteCache, useGlobalCache, generateDocs)
            .get();
      } catch (InterruptedException e) {
        throw new RuntimeException(e);
      } catch (ExecutionException e) {
        var re = new RuntimeException(e);
        re.setStackTrace(e.getStackTrace());
        throw re;
      }
    }

    @Specialization
    static Object doInvoke(
        TopLevelScope scope, String member, Object[] arguments, @Bind("$node") Node node)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      var ctx = EnsoContext.get(node);
      switch (member) {
        case MethodNames.TopScope.GET_MODULE:
          return getModule(scope, arguments);
        case MethodNames.TopScope.CREATE_MODULE:
          return createModule(scope, arguments, ctx);
        case MethodNames.TopScope.REGISTER_MODULE:
          return registerModule(scope, arguments, ctx);
        case MethodNames.TopScope.UNREGISTER_MODULE:
          return unregisterModule(scope, arguments, ctx);
        case MethodNames.TopScope.LEAK_CONTEXT:
          return leakContext(ctx);
        case MethodNames.TopScope.COMPILE:
          return compile(arguments, ctx);
        case MethodNames.TopScope.FIND_NATIVE_LIBRARY:
          return findNativeLibrary(arguments, ctx);
        default:
          throw UnknownIdentifierException.create(member);
      }
    }
  }

  /**
   * Checks if a member can be invoked through the polyglot API.
   *
   * @param member the member name.
   * @return {@code true} if the member is invocable, {@code false} otherwise.
   */
  @ExportMessage
  boolean isMemberInvocable(String member) {
    return member.equals(MethodNames.TopScope.GET_MODULE)
        || member.equals(MethodNames.TopScope.CREATE_MODULE)
        || member.equals(MethodNames.TopScope.REGISTER_MODULE)
        || member.equals(MethodNames.TopScope.UNREGISTER_MODULE)
        || member.equals(MethodNames.TopScope.LEAK_CONTEXT)
        || member.equals(MethodNames.TopScope.COMPILE)
        || member.equals(MethodNames.TopScope.FIND_NATIVE_LIBRARY);
  }

  /**
   * Checks if the receiver is a scope object.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean isScope() {
    return true;
  }

  /**
   * Returns true if this scope has an enclosing parent scope, else false.
   *
   * @return {@code false}
   */
  @ExportMessage
  boolean hasScopeParent() {
    return false;
  }

  /**
   * Returns the parent scope of this scope, if one exists.
   *
   * @return {@code null}
   * @throws UnsupportedMessageException always, as this scope can never have a parent
   */
  @ExportMessage
  Object getScopeParent() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  /**
   * Converts this scope to a human readable string.
   *
   * @param allowSideEffects whether or not side effects are allowed
   * @return a string representation of this scope
   */
  @ExportMessage
  @Override
  public Object toDisplayString(boolean allowSideEffects) {
    return "Enso.Top_Scope";
  }
}
