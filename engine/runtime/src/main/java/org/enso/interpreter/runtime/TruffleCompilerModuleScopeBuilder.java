package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;
import org.enso.compiler.context.CompilerContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.RedefinedConversionException;
import org.enso.interpreter.runtime.error.RedefinedMethodException;
import org.enso.interpreter.runtime.scope.ImportExportScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.util.CachingSupplier;

/**
 * Builder to create an instance of {@link ModuleScope}. Implements the {@link CompilerContext}'s
 * module scope builder interface, but adds Truffle related functionality to {@code IrToTruffle} &
 * co. to build up necessary runtime structures.
 */
final class TruffleCompilerModuleScopeBuilder extends CompilerContext.ModuleScopeBuilder
    implements ModuleScopeBuilder {
  private final Consumer<ModuleScope> onFinish;
  private ModuleScope moduleScope;
  private final Module module;
  private final Type associatedType;
  private final Map<String, Supplier<TruffleObject>> polyglotSymbols;
  private final Map<String, Type> types;
  private final Map<Type, Map<String, Supplier<Function>>> methods;
  private final Map<Type, Map<Type, Supplier<Function>>> conversions;
  private final Set<ImportExportScope> imports;
  private final Set<ImportExportScope> exports;

  TruffleCompilerModuleScopeBuilder(Module module, Consumer<ModuleScope> onFinish) {
    this.module = module;
    this.onFinish = onFinish;
    this.polyglotSymbols = new LinkedHashMap<>();
    this.types = new LinkedHashMap<>();
    this.methods = new LinkedHashMap<>();
    this.conversions = new LinkedHashMap<>();
    this.imports = new LinkedHashSet<>();
    this.exports = new LinkedHashSet<>();
    this.associatedType = Type.createSingleton(module.getName().item(), this, null, false, false);
  }

  static TruffleCompilerModuleScopeBuilder fromCompilerModuleScopeBuilder(
      CompilerContext.ModuleScopeBuilder scopeBuilder) {
    return (TruffleCompilerModuleScopeBuilder) scopeBuilder;
  }

  static TruffleCompilerModuleScopeBuilder fromCompilerModule(CompilerContext.Module module) {
    return fromCompilerModuleScopeBuilder(module.getScopeBuilder());
  }

  @Override
  public Type registerType(Type type) {
    assert moduleScope == null;
    Type current = types.putIfAbsent(type.getName(), type);
    return current == null ? type : current;
  }

  @Override
  public TruffleCompilerModuleScopeBuilder toCompilerBuilder() {
    return this;
  }

  /**
   * Returns a map of methods defined in this module for a given type.
   *
   * @param type the type for which method map is requested
   * @return a map containing all the defined methods by name
   */
  private Map<String, Supplier<Function>> ensureMethodMapFor(Type type) {
    Type tpeKey = type == null ? Type.noType() : type;
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
  @Override
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
  void registerConversionMethod(Type toType, Type fromType, Function function) {
    registerConversionMethod(toType, fromType, () -> function);
  }

  void registerConversionMethod(Type toType, Type fromType, Supplier<Function> supply) {
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
  void registerPolyglotSymbol(String name, Supplier<TruffleObject> symbolFactory) {
    assert moduleScope == null;
    polyglotSymbols.put(name, CachingSupplier.wrap(symbolFactory));
  }

  /**
   * Registers all methods of a type in the provided scope.
   *
   * @param tpe the methods of which type should be registered
   * @param scope target scope where methods should be registered to
   */
  @Override
  public void registerAllMethodsOfTypeToScope(Type tpe, ModuleScopeBuilder scope) {
    // FIXME: because of Builtins can't enable 'assert moduleScope == null;'
    Type tpeKey = tpe == null ? Type.noType() : tpe;
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
  void addImport(ImportExportScope scope) {
    assert moduleScope == null;
    imports.add(scope);
  }

  /**
   * Adds an information about the module exporting another module.
   *
   * @param scope the exported scope
   */
  void addExport(ImportExportScope scope) {
    assert moduleScope == null;
    exports.add(scope);
  }

  @Override
  public Module getModule() {
    return module;
  }

  /** Complete building this scope */
  @Override
  public void finish() {
    if (moduleScope == null) {
      moduleScope = createModuleScope();
      onFinish.accept(moduleScope);
    }
  }

  @Override
  public Type getAssociatedType() {
    return associatedType;
  }

  @Override
  public Type getType(String name, boolean ignoreAssociatedType) {
    if (!ignoreAssociatedType && associatedType.getName().equals(name)) {
      return associatedType;
    }
    return types.get(name);
  }

  public Supplier<TruffleObject> getPolyglotSymbolSupplier(String symbolName) {
    return ModuleScopeAccessor.getInstance()
        .findPolyglotSymbolSupplier(polyglotSymbols, symbolName);
  }

  public Function getMethodForType(Type tpe, String name) {
    return ModuleScopeAccessor.getInstance().findMethodForType(tpe, methods, name);
  }

  @Override
  public ModuleScope asModuleScope() {
    if (moduleScope != null) {
      return moduleScope;
    } else {
      throw CompilerDirectives.shouldNotReachHere("build() first!");
    }
  }

  @CompilerDirectives.TruffleBoundary
  private ModuleScope createModuleScope() {
    return ModuleScopeAccessor.getInstance()
        .newModuleScope(
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
