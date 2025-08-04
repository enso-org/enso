package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import org.enso.compiler.context.CompilerContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.RedefinedConversionException;
import org.enso.interpreter.runtime.error.RedefinedMethodException;
import org.enso.interpreter.runtime.util.CachingSupplier;

/** Builder to create an instance of {@link ModuleScope}. */
public final class ModuleScopeBuilder {

  @CompilerDirectives.CompilationFinal private ModuleScope moduleScope = null;
  private final Module module;
  private final Type associatedType;
  private final Map<String, Supplier<TruffleObject>> polyglotSymbols;
  private final Map<String, Type> types;
  private final Map<Type, Map<String, Supplier<Function>>> methods;
  private final Map<Type, Map<Type, Supplier<Function>>> conversions;
  private final Set<ImportExportScope> imports;
  private final Set<ImportExportScope> exports;

  public ModuleScopeBuilder(Module module) {
    this.module = module;
    this.polyglotSymbols = new LinkedHashMap<>();
    this.types = new LinkedHashMap<>();
    this.methods = new LinkedHashMap<>();
    this.conversions = new LinkedHashMap<>();
    this.imports = new LinkedHashSet<>();
    this.exports = new LinkedHashSet<>();
    this.associatedType = Type.createSingleton(module.getName().item(), this, null, false, false);
  }

  public ModuleScopeBuilder(Module module, Map<String, Type> types) {
    this.module = module;
    this.polyglotSymbols = new LinkedHashMap<>();
    this.types = types;
    this.methods = new LinkedHashMap<>();
    this.conversions = new LinkedHashMap<>();
    this.imports = new LinkedHashSet<>();
    this.exports = new LinkedHashSet<>();
    this.associatedType = Type.createSingleton(module.getName().item(), this, null, false, false);
  }

  private ModuleScopeBuilder(
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

  public Type registerType(Type type) {
    assert moduleScope == null;
    Type current = types.putIfAbsent(type.getName(), type);
    return current == null ? type : current;
  }

  /**
   * Returns a map of methods defined in this module for a given type.
   *
   * @param type the type for which method map is requested
   * @return a map containing all the defined methods by name
   */
  private Map<String, Supplier<Function>> ensureMethodMapFor(Type type) {
    Type tpeKey = type == null ? ModuleScopeUtils.noTypeKey : type;
    return methods.computeIfAbsent(tpeKey, k -> new LinkedHashMap<>());
  }

  /**
   * Registers a method defined for a given type.
   *
   * @param type the type the method was defined for
   * @param method method name
   * @param function the {@link Function} associated with this definition
   */
  public void registerMethod(Type type, String method, Function function) {
    assert moduleScope == null;
    Map<String, Supplier<Function>> methodMap = ensureMethodMapFor(type);
    // Builtin types will have double definition because of
    // BuiltinMethod and that's OK
    if (methodMap.containsKey(method) && !type.isBuiltin()) {
      throw new RedefinedMethodException(type.getName(), method);
    } else {
      methodMap.put(method, CachingSupplier.forValue(function));
    }
  }

  /**
   * Registers a lazily constructed method defined for a given type.
   *
   * @param type the type the method was defined for
   * @param method method name
   * @param supply provider of the {@link Function} associated with this definition
   */
  public void registerMethod(Type type, String method, Supplier<Function> supply) {
    assert moduleScope == null;
    Map<String, Supplier<Function>> methodMap = ensureMethodMapFor(type);
    // Builtin types will have double definition because of
    // BuiltinMethod and that's OK
    if (methodMap.containsKey(method) && !type.isBuiltin()) {
      throw new RedefinedMethodException(type.getName(), method);
    } else {
      methodMap.put(method, CachingSupplier.wrap(supply));
    }
  }

  /**
   * Registers a conversion method for a given type
   *
   * @param toType type the conversion was defined to
   * @param fromType type the conversion was defined from
   * @param function the {@link Function} associated with this definition
   */
  public void registerConversionMethod(Type toType, Type fromType, Function function) {
    registerConversionMethod(toType, fromType, () -> function);
  }

  public void registerConversionMethod(Type toType, Type fromType, Supplier<Function> supply) {
    assert moduleScope == null;
    java.util.Map<
            org.enso.interpreter.runtime.data.Type,
            java.util.function.Supplier<org.enso.interpreter.runtime.callable.function.Function>>
        sourceMap = conversions.computeIfAbsent(toType, k -> new LinkedHashMap<>());
    if (sourceMap.containsKey(fromType)) {
      throw new RedefinedConversionException(toType.getName(), fromType.getName());
    } else {
      sourceMap.put(fromType, CachingSupplier.wrap(supply));
    }
  }

  /**
   * Registers a new symbol in the polyglot namespace.
   *
   * @param name the name of the symbol
   * @param symbolFactory the value being exposed
   */
  public void registerPolyglotSymbol(String name, Supplier<TruffleObject> symbolFactory) {
    assert moduleScope == null;
    polyglotSymbols.put(name, CachingSupplier.wrap(symbolFactory));
  }

  /**
   * Registers all methods of a type in the provided scope.
   *
   * @param tpe the methods of which type should be registered
   * @param scope target scope where methods should be registered to
   */
  public void registerAllMethodsOfTypeToScope(Type tpe, ModuleScopeBuilder scope) {
    // FIXME: because of Builtins can't enable 'assert moduleScope == null;'
    Type tpeKey = tpe == null ? ModuleScopeUtils.noTypeKey : tpe;
    java.util.Map<
            java.lang.String,
            java.util.function.Supplier<org.enso.interpreter.runtime.callable.function.Function>>
        allTypeMethods = methods.get(tpeKey);
    if (allTypeMethods != null) {
      allTypeMethods.forEach((name, fun) -> scope.registerMethod(tpeKey, name, fun));
    }
  }

  /**
   * Adds a dependency for this module.
   *
   * @param scope the scope of the newly added dependency
   */
  public void addImport(ImportExportScope scope) {
    assert moduleScope == null;
    imports.add(scope);
  }

  /**
   * Adds an information about the module exporting another module.
   *
   * @param scope the exported scope
   */
  public void addExport(ImportExportScope scope) {
    assert moduleScope == null;
    exports.add(scope);
  }

  public Module getModule() {
    return module;
  }

  /**
   * Create a new ModuleScopeBuilder which inherits from `this` `module` and `types` that need to
   * survive the compilation.
   *
   * @return new ModuleScopeBuilder
   */
  public ModuleScopeBuilder newBuilderInheritingTypes() {
    return new ModuleScopeBuilder(this.module, new LinkedHashMap<>(this.types));
  }

  /**
   * Materializes the builder and ensures that no further modifications to ModuleScope are possible.
   * Action is idempotent.
   *
   * @return an immutable ModuleScope
   */
  public ModuleScope build() {
    if (moduleScope == null) {
      moduleScope =
          new ModuleScope(
              module,
              associatedType,
              Collections.unmodifiableMap(polyglotSymbols),
              Collections.unmodifiableMap(types),
              Collections.unmodifiableMap(methods),
              Collections.unmodifiableMap(conversions),
              Collections.unmodifiableSet(imports),
              Collections.unmodifiableSet(exports));
    }
    return moduleScope;
  }

  public Type getAssociatedType() {
    return associatedType;
  }

  public Type getType(String name, boolean ignoreAssociatedType) {
    if (!ignoreAssociatedType && associatedType.getName().equals(name)) {
      return associatedType;
    }
    return types.get(name);
  }

  public Supplier<TruffleObject> getPolyglotSymbolSupplier(String symbolName) {
    return ModuleScopeUtils.findPolyglotSymbolSupplier(polyglotSymbols, symbolName);
  }

  public Function getMethodForType(Type tpe, String name) {
    return ModuleScopeUtils.findMethodForType(tpe, methods, name);
  }

  public static ModuleScopeBuilder fromCompilerModuleScopeBuilder(
      CompilerContext.ModuleScopeBuilder scopeBuilder) {
    return ((TruffleCompilerModuleScopeBuilder) scopeBuilder).unsafeScopeBuilder();
  }

  /**
   * Return a view on `this` as a ModuleScope, rather than its builder.
   *
   * @return ModuleScope, if the builder has already been `built`, a proxy instance with the
   *     currently registered entities
   */
  public ModuleScope asModuleScope() {
    if (moduleScope != null) {
      return moduleScope;
    } else {
      throw CompilerDirectives.shouldNotReachHere("build() first!");
    }
  }

  @CompilerDirectives.TruffleBoundary
  private ModuleScope createModuleScope() {
    return new ModuleScope(
        module,
        associatedType,
        Collections.unmodifiableMap(polyglotSymbols),
        Collections.unmodifiableMap(types),
        Collections.unmodifiableMap(methods),
        Collections.unmodifiableMap(conversions),
        Collections.unmodifiableSet(imports),
        Collections.unmodifiableSet(exports));
  }

  @Override
  public java.lang.String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ModuleScope builder for " + module.getName());
    builder.append(",\n");
    builder.append("Polyglot Symbols: " + polyglotSymbols);
    builder.append(",\n");
    builder.append("Methods: " + methods);
    builder.append(",\n");
    builder.append("Conversions: " + conversions);
    builder.append(",\n");
    builder.append("Imports: " + imports);
    builder.append(",\n");
    builder.append("Exports: " + exports);
    builder.append(",\n");
    builder.append("Types: " + types);
    return builder.toString();
  }
}
