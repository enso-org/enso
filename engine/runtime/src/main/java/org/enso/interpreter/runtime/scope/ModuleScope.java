package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.enso.compiler.common.MethodResolutionAlgorithm;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.ModuleScopeAccessor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** A representation of Enso's per-file top-level scope. */
@ExportLibrary(TypesLibrary.class)
public final class ModuleScope extends EnsoObject {
  private static final ModuleScopeAccessor IMPL =
      new ModuleScopeAccessor() {
        @Override
        protected Function findMethodForType(
            Type tpe, Map<Type, Map<String, Supplier<Function>>> m, String name) {
          return ModuleScopeUtils.findMethodForType(tpe, m, name);
        }

        @Override
        protected Supplier<TruffleObject> findPolyglotSymbolSupplier(
            Map<String, Supplier<TruffleObject>> ps, String symbolName) {
          return ModuleScopeUtils.findPolyglotSymbolSupplier(ps, symbolName);
        }

        @Override
        protected ModuleScope newModuleScope(
            Module module,
            Type associatedType,
            Map<String, Supplier<TruffleObject>> m1,
            Map<String, Type> m2,
            Map<Type, Map<String, Supplier<Function>>> m3,
            Map<Type, Map<Type, Supplier<Function>>> m4,
            Set<ImportExportScope> s1,
            Set<ImportExportScope> s2) {
          return new ModuleScope(module, associatedType, m1, m2, m3, m4, s1, s2);
        }
      };
  private final Type associatedType;
  private final Module module;
  private final Map<String, Supplier<TruffleObject>> polyglotSymbols;
  private final Map<String, Type> types;
  private final Map<Type, Map<String, Supplier<Function>>> methods;

  /**
   * First key is target type, second key is source type. The value is the conversion function from
   * source to target.
   */
  private final Map<Type, Map<Type, Supplier<Function>>> conversions;

  private final Set<ImportExportScope> imports;
  private final Set<ImportExportScope> exports;

  ModuleScope(
      Module module,
      Type associatedType,
      Map<String, Supplier<TruffleObject>> polyglotSymbols,
      Map<String, Type> types,
      Map<Type, Map<String, Supplier<Function>>> methods,
      Map<Type, Map<Type, Supplier<Function>>> conversions,
      Set<ImportExportScope> imports,
      Set<ImportExportScope> exports) {
    this.module = module;
    this.associatedType = associatedType;
    this.polyglotSymbols = polyglotSymbols;
    this.types = types;
    this.methods = methods;
    this.conversions = conversions;
    this.imports = imports;
    this.exports = exports;
  }

  /**
   * @return the associated type of this module.
   */
  public Type getAssociatedType() {
    return associatedType;
  }

  /**
   * @return the module associated with this scope.
   */
  public Module getModule() {
    return module;
  }

  /**
   * Looks up the definition for a given type and method name.
   *
   * <p>The resolution algorithm is first looking for methods defined at the constructor definition
   * site (i.e. non-overloads), then looks for methods defined in this scope and finally tries to
   * resolve the method in all dependencies of this module.
   *
   * @param type type to lookup the method for.
   * @param name the method name.
   * @return the matching method definition or null if not found.
   */
  @CompilerDirectives.TruffleBoundary
  public Function lookupMethodDefinition(Type type, String name) {
    assert !getModule().needsCompilation();
    return RuntimeMethodResolution.INSTANCE.lookupMethodDefinition(this, type, name);
  }

  private static final class RuntimeMethodResolution
      extends MethodResolutionAlgorithm<Function, Type, ImportExportScope, ModuleScope> {
    private static final RuntimeMethodResolution INSTANCE = new RuntimeMethodResolution();

    @Override
    protected Collection<ImportExportScope> getImportsFromModuleScope(ModuleScope moduleScope) {
      return moduleScope.getImports();
    }

    @Override
    protected Collection<ImportExportScope> getExportsFromModuleScope(ModuleScope moduleScope) {
      return moduleScope.getExports();
    }

    @Override
    protected Function getConversionFromModuleScope(
        ModuleScope moduleScope, Type target, Type source) {
      return moduleScope.getConversionFor(target, source);
    }

    @Override
    protected Function getMethodFromModuleScope(
        ModuleScope moduleScope, Type type, String methodName) {
      return moduleScope.getMethodForType(type, methodName);
    }

    @Override
    protected ModuleScope findDefinitionScope(Type type) {
      return type.getDefinitionScope();
    }

    @Override
    protected Function getMethodForTypeFromScope(
        ImportExportScope scope, Type type, String methodName) {
      return scope.getMethodForType(type, methodName);
    }

    @Override
    protected Function getExportedMethodFromScope(
        ImportExportScope scope, Type type, String methodName) {
      return scope.getExportedMethod(type, methodName);
    }

    @Override
    protected Function getConversionFromScope(ImportExportScope scope, Type target, Type source) {
      return scope.getConversionForType(target, source);
    }

    @Override
    protected Function getExportedConversionFromScope(
        ImportExportScope scope, Type target, Type source) {
      return scope.getExportedConversion(target, source);
    }

    @Override
    protected Function onMultipleDefinitionsFromImports(
        String methodName, List<MethodFromImport<Function, ImportExportScope>> methodFromImports) {
      assert !methodFromImports.isEmpty();
      return methodFromImports.get(0).resolutionResult();
    }
  }

  public Collection<ImportExportScope> getImports() {
    return imports;
  }

  public Collection<ImportExportScope> getExports() {
    return exports;
  }

  /**
   * Looks up a conversion method from source type to target type. The conversion method
   * implementation looks like this:
   *
   * <pre>
   *   Target_Type.from (other : Source_Type) = ...
   * </pre>
   *
   * The conversion method is first looked up in the scope of the source type, then in the scope of
   * the target type and finally in all the imported scopes.
   *
   * @param source Source type
   * @param target Target type
   * @return The conversion method or null if not found.nie
   */
  @CompilerDirectives.TruffleBoundary
  public Function lookupConversionDefinition(Type source, Type target) {
    assert !getModule().needsCompilation();
    return RuntimeMethodResolution.INSTANCE.lookupConversionDefinition(this, source, target);
  }

  Function getExportedMethod(Type type, String name) {
    return RuntimeMethodResolution.INSTANCE.getExportedMethod(this, type, name);
  }

  Function getExportedConversion(Type target, Type source) {
    return RuntimeMethodResolution.INSTANCE.getExportedConversion(this, target, source);
  }

  public List<Type> getAllTypes(String name) {
    var tpes = new ArrayList<Type>(types.size() + 1);
    var tpe0 = getType(name, false);
    if (tpe0 != null) tpes.add(tpe0);
    tpes.addAll(types.values());
    return tpes;
  }

  public List<Type> getAllTypes() {
    return types.values().stream().collect(Collectors.toUnmodifiableList());
  }

  @ExportMessage.Ignore
  public Type getType(String name, boolean ignoreAssociatedType) {
    if (!ignoreAssociatedType && associatedType.getName().equals(name)) {
      return associatedType;
    }
    return types.get(name);
  }

  @ExportMessage.Ignore
  public boolean hasType(Type type) {
    return types.get(type.getName()) == type;
  }

  /**
   * @return a method for the given type
   */
  public Function getMethodForType(Type tpe, String name) {
    return ModuleScopeUtils.findMethodForType(tpe, methods, name);
  }

  /**
   * Returns the names of methods for the given type.
   *
   * @param tpe the type in the scope. If null, treated as {@code noType}.
   * @return names of methods or null
   */
  public Set<String> getMethodNamesForType(Type tpe) {
    Type tpeKey = tpe == null ? ModuleScopeUtils.noTypeKey : tpe;
    var allTpeMethods = methods.get(tpeKey);
    return allTpeMethods == null ? null : allTpeMethods.keySet();
  }

  /**
   * Returns a set of all the functions for a type, or null.
   *
   * @param tpe the type in the scope. If null, treated as {@code noType}.
   * @return set of methods or null.
   */
  public Set<Function> getMethodsForType(Type tpe) {
    Type tpeKey = tpe == null ? ModuleScopeUtils.noTypeKey : tpe;
    var allTpeMethods = methods.get(tpeKey);
    if (allTpeMethods != null) {
      return allTpeMethods.values().stream().map(Supplier::get).collect(Collectors.toSet());
    } else {
      return null;
    }
  }

  public Function getConversionFor(Type target, Type source) {
    var conversionsOnType = conversions.get(target);
    if (conversionsOnType == null) {
      return null;
    }
    var supply = conversionsOnType.get(source);
    return supply == null ? null : supply.get();
  }

  /**
   * @return methods for all registered types
   */
  public List<Function> getAllMethods() {
    return methods.values().stream()
        .flatMap(e -> e.values().stream())
        .map(s -> s.get())
        .collect(Collectors.toList());
  }

  /**
   * @return the raw conversions held by this module
   */
  public List<Function> getConversions() {
    return conversions.values().stream()
        .flatMap(e -> e.values().stream())
        .map(s -> s.get())
        .collect(Collectors.toList());
  }

  /**
   * Finds a polyglot symbol supplier. The supplier will then load the provided {@code symbolName}
   * when its {@link Supplier#get()} method is called.
   *
   * @param symbolName name of the symbol to search for
   * @return non-{@code null} supplier of a polyglot symbol imported into this scope
   */
  public Supplier<TruffleObject> getPolyglotSymbolSupplier(String symbolName) {
    return ModuleScopeUtils.findPolyglotSymbolSupplier(polyglotSymbols, symbolName);
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType() {
    return getAssociatedType();
  }

  @Override
  public String toString() {
    return "Scope" + module;
  }

  @Override
  @TruffleBoundary
  public Object toDisplayString(boolean allowSideEffects) {
    return toString();
  }
}
